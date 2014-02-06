/* libutf8/src/libutf8/300_encode.c
 *
 *  (c)2006-2009, Laurence Withers, <l@lwithers.me.uk>.
 *  Released under the GNU GPLv3. See file COPYING or
 *  http://www.gnu.org/copyleft/gpl.html for details.
*/



char*
utf8_encode_char(char* dest, size_t amt, wchar_t ch)
{
    if(!dest || !amt) {
        errno = EINVAL;
        return 0;
    }
    if(!utf8_isucs4(ch)) {
        errno = EILSEQ;
        return 0;
    }

    if(ch < 0x80) {
        *dest++ = ch;

    } else if(ch < 0x800) {
        if(amt < 2) {
            errno = ENOMEM;
            return 0;
        }
        *dest++ = 0xC0 | ((ch >> 6) & 0x1F);
        *dest++ = 0x80 | (ch & 0x3F);

    } else if(ch < 0x10000) {
        if(amt < 3) {
            errno = ENOMEM;
            return 0;
        }
        *dest++ = 0xE0 | ((ch >> 12) & 0xF);
        *dest++ = 0x80 | ((ch >> 6) & 0x3F);
        *dest++ = 0x80 | (ch & 0x3F);

    } else if(ch < 0x200000) {
        if(amt < 4) {
            errno = ENOMEM;
            return 0;
        }
        *dest++ = 0xF0 | ((ch >> 18) & 0x7);
        *dest++ = 0x80 | ((ch >> 12) & 0x3F);
        *dest++ = 0x80 | ((ch >> 6) & 0x3F);
        *dest++ = 0x80 | (ch & 0x3F);

    } else if(ch < 0x4000000) {
        if(amt < 5) {
            errno = ENOMEM;
            return 0;
        }
        *dest++ = 0xF8 | ((ch >> 24) & 0x3);
        *dest++ = 0x80 | ((ch >> 18) & 0x3F);
        *dest++ = 0x80 | ((ch >> 12) & 0x3F);
        *dest++ = 0x80 | ((ch >> 6) & 0x3F);
        *dest++ = 0x80 | (ch & 0x3F);

    } else {
        if(amt < 6) {
            errno = ENOMEM;
            return 0;
        }
        *dest++ = 0xFC | ((ch >> 30) & 0x1);
        *dest++ = 0x80 | ((ch >> 24) & 0x3F);
        *dest++ = 0x80 | ((ch >> 18) & 0x3F);
        *dest++ = 0x80 | ((ch >> 12) & 0x3F);
        *dest++ = 0x80 | ((ch >> 6) & 0x3F);
        *dest++ = 0x80 | (ch & 0x3F);

    }

    return dest;
}



char*
utf8_encode_char_force(char* dest, size_t amt, wchar_t ch, wchar_t ilseq)
{
    if(!utf8_isucs4(ilseq)) {
        errno = EILSEQ;
        return 0;
    }

    return utf8_encode_char(dest, amt, utf8_isucs4(ch) ? ch : ilseq);
}



char*
utf8_encode(char* dest, size_t amt, const wchar_t* src)
{
    return utf8_encode2(dest, amt, 0, src, -1);
}



char*
utf8_encode2(char* dest, size_t amt, size_t* written, const wchar_t* src,
    size_t inamt)
{
    struct utf8_encode_state ctx;
    memset(&ctx, 0, sizeof(ctx));
    ctx.rd = src;
    ctx.rd_remain = inamt;
    ctx.wr = dest;
    ctx.wr_size = amt;

    if(!utf8_encoder(&ctx)) return 0;
    if(ctx.rd_remain > 0 || (ctx.rd_remain < 0 && *ctx.rd)) {
        errno = ENOMEM;
        return 0;
    }
    if(written) *written = ctx.written;
    return dest;
}



char*
utf8_encode_force(char* dest, size_t amt, const wchar_t* src)
{
    return utf8_encode_force2(dest, amt, 0, src, -1);
}



char*
utf8_encode_force2(char* dest, size_t amt, size_t* written, const wchar_t* src,
    size_t inamt)
{
    struct utf8_encode_state ctx;
    memset(&ctx, 0, sizeof(ctx));
    ctx.rd = src;
    ctx.rd_remain = inamt;
    ctx.wr = dest;
    ctx.wr_size = amt;
    ctx.error_callback = utf8_encode_error_callback_replace;

    if(!utf8_encoder(&ctx)) return 0;
    if(written) *written = ctx.written;
    return dest;
}



/* options for text editors
kate: replace-trailing-space-save true; space-indent true; tab-width 4;
vim: expandtab:ts=4:sw=4:syntax=c.doxygen
*/
