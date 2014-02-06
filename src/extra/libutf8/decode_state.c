/* libutf8/src/libutf8/400_decode_state.c
 *
 *  (c)2006-2009, Laurence Withers, <l@lwithers.me.uk>.
 *  Released under the GNU GPLv3. See file COPYING or
 *  http://www.gnu.org/copyleft/gpl.html for details.
*/



enum utf8_decoder_state {
    utf8_state_none,
    utf8_state_multibyte1,
    utf8_state_multibyte2,
    utf8_state_multibyte3,
    utf8_state_multibyte4,
    utf8_state_multibyte5,
    utf8_state_error,
    utf8_state_skip
};



struct utf8_decode_state*
utf8_decoder(struct utf8_decode_state* ctx)
{
    wchar_t* wr;
    size_t avail;
    enum utf8_decode_error error_type;

    if(!ctx || !ctx->rd || !ctx->wr || ctx->wr_size < 2 || ctx->state == utf8_state_error) {
        errno = EINVAL;
        return 0;
    }

    wr = ctx->wr;
    ctx->written = 0;
    avail = ctx->wr_size;

  loop:
    while(ctx->rd_remain) {
        uint8_t in = *ctx->rd;

        switch(ctx->state) {
        case utf8_state_skip:
        case utf8_state_none:
            if(!in && ctx->rd_remain < 0) {
                *wr = 0;
                ctx->complete = 1;
                ++ctx->byte_offset;
                return ctx;
            }
            if(!(in & 0x80)) {
                *wr++ = in;
                ++ctx->written;
                --avail;
                ++ctx->char_offset;
                ctx->complete = 1;
                if(in == 0x0A) {
                    ++ctx->line;
                    ctx->col = 0;
                } else {
                    ++ctx->col;
                }
                ctx->state = utf8_state_none;
                break;
            }
            ctx->complete = 0;
            if((in & 0xE0) == 0xC0) {
                ctx->minch = 0x80;
                ctx->state = utf8_state_multibyte1;
                ctx->statech = in & 0x1F;
            } else if((in & 0xF0) == 0xE0) {
                ctx->minch = 0x800;
                ctx->state = utf8_state_multibyte2;
                ctx->statech = in & 0x0F;
            } else if((in & 0xF8) == 0xF0) {
                ctx->minch = 0x10000;
                ctx->state = utf8_state_multibyte3;
                ctx->statech = in & 0x07;
            } else if((in & 0xFC) == 0xF8) {
                ctx->minch = 0x200000;
                ctx->state = utf8_state_multibyte4;
                ctx->statech = in & 0x03;
            } else if((in & 0xFE) == 0xFC) {
                ctx->minch = 0x4000000;
                ctx->state = utf8_state_multibyte5;
                ctx->statech = in & 0x01;
            } else if(ctx->state != utf8_state_none) {
                ctx->state = utf8_state_none;
            } else {
                error_type = ((in & 0xC0) == 0x80) ? utf8_decode_error_lone_cchar
                    : utf8_decode_error_not_schar;
                goto error;
            }
            break;

        case utf8_state_multibyte1:
        case utf8_state_multibyte2:
        case utf8_state_multibyte3:
        case utf8_state_multibyte4:
        case utf8_state_multibyte5:
            if((in & 0xC0) != 0x80) {
                error_type = utf8_decode_error_not_cchar;
                goto error;
            }
            ctx->statech <<= 6;
            ctx->statech |= in & 0x3F;
            if(!--ctx->state) {
                if(ctx->statech < ctx->minch) {
                    error_type = utf8_decode_error_overlong;
                    goto error;
                } else {
                    /* validate codepoint */
                    if(!utf8_isucs4(ctx->statech)) {
                        error_type = utf8_decode_error_illegal_cp;
                        goto error;
                    }

                    /* add to output string */
                    *wr++ = ctx->statech;
                    ++ctx->written;
                    --avail;
                    ++ctx->char_offset;
                    ctx->complete = 1;
                    if(ctx->statech == 0x0A || ctx->statech == 0x2028) {
                        ++ctx->line;
                        ctx->col = 0;
                    } else {
                        ++ctx->col;
                    }
               }
            }
            break;

        default:
            errno = EINVAL;
            return 0;
        }

        ++ctx->byte_offset;
        ++ctx->rd;
        if(ctx->rd_remain > 0) --ctx->rd_remain;
        if(avail == 1) break;
    }
    *wr = 0;
    return ctx;

  error:
    if(!ctx->error_callback) {
        errno = EILSEQ;
        return 0;
    }
    switch(ctx->error_callback(ctx, error_type, wr)) {
    case utf8_decode_error_action_abort:
        errno = EILSEQ;
        return 0;

    case utf8_decode_error_action_skip:
        ctx->state = utf8_state_skip;
        goto loop;

    case utf8_decode_error_action_replace:
        ctx->state = utf8_state_skip;
        ++ctx->written;
        if(*wr == 0x0A || *wr == 0x2028) {
            ++ctx->line;
            ctx->col = 0;
        } else {
            ++ctx->col;
        }
        ++wr;
        if(--avail == 1) {
            *wr = 0;
            return ctx;
        }
        goto loop;
    }

    /* shouldn't reach here */
    errno = EILSEQ;
    return 0;
}



enum utf8_decode_error_action
utf8_decode_error_callback_replace(const struct utf8_decode_state* ctx,
    enum utf8_decode_error error, wchar_t* newch)
{
    (void)ctx;
    (void)error;
    *newch = 0xFFFD;
    return utf8_decode_error_action_replace;
}



enum utf8_decode_error_action
utf8_decode_error_callback_skip(const struct utf8_decode_state* ctx,
    enum utf8_decode_error error, wchar_t* newch)
{
    (void)ctx;
    (void)error;
    (void)newch;
    return utf8_decode_error_action_skip;
}
