/* libutf8/src/libutf8/200_decode.c
 *
 *  (c)2006-2009, Laurence Withers, <l@lwithers.me.uk>.
 *  Released under the GNU GPLv3. See file COPYING or
 *  http://www.gnu.org/copyleft/gpl.html for details.
*/



wchar_t
utf8_decode_char(const char* src, size_t* used)
{
    return utf8_decode_char2(src, 6, used);
}



wchar_t
utf8_decode_char2(const char* src, size_t size, size_t* used)
{
    uint8_t ch;
    wchar_t ret, min;
    int remain;

    if(!src || !size) {
        errno = EINVAL;
        return (wchar_t)-1;
    }
    if(used) *used = 1;
    ch = *src++;

    if(ch & 0x80) {
        if((ch & 0xE0) == 0xC0) {
            min = 0x80;
            remain = 1;
            if(used) *used = 2;
            ret = ch & 0x1F;
        } else if((ch & 0xF0) == 0xE0) {
            min = 0x800;
            remain = 2;
            if(used) *used = 3;
            ret = ch & 0x0F;
        } else if((ch & 0xF8) == 0xF0) {
            min = 0x10000;
            remain = 3;
            if(used) *used = 4;
            ret = ch & 0x07;
        } else if((ch & 0xFC) == 0xF8) {
            min = 0x200000;
            remain = 4;
            if(used) *used = 5;
            ret = ch & 0x03;
        } else if((ch & 0xFE) == 0xFC) {
            min = 0x4000000;
            remain = 5;
            if(used) *used = 6;
            ret = ch & 0x01;
        } else {
            errno = EILSEQ;
            return (wchar_t)-1;
        }

        while(remain--) {
            if(!--size) {
                errno = EILSEQ;
                return (wchar_t)-1;
            }
            ch = *src++;
            if((ch & 0xC0) != 0x80) {
                errno = EILSEQ;
                return (wchar_t)-1;
            }
            ret <<= 6;
            ret |= ch & 0x3F;
        }

        if(ret < min) {
            errno = EILSEQ;
            return (wchar_t)-1;
        }

        return ret;
    }
    return ch;
}



wchar_t
utf8_decode_char_force(const char* src, size_t* used, wchar_t ilseq)
{
    return utf8_decode_char2_force(src, 6, used, ilseq);
}



wchar_t
utf8_decode_char2_force(const char* src, size_t size, size_t* used,
    wchar_t ilseq)
{
    uint8_t ch;
    wchar_t ret, min;
    int remain;

    if(!src || !size) {
        errno = EINVAL;
        return (wchar_t)-1;
    }
    if(used) *used = 1;
    ch = *src++;

    if(ch & 0x80) {
        if((ch & 0xE0) == 0xC0) {
            min = 0x80;
            remain = 1;
            ret = ch & 0x1F;
        } else if((ch & 0xF0) == 0xE0) {
            min = 0x800;
            remain = 2;
            ret = ch & 0x0F;
        } else if((ch & 0xF8) == 0xF0) {
            min = 0x10000;
            remain = 3;
            ret = ch & 0x07;
        } else if((ch & 0xFC) == 0xF8) {
            min = 0x200000;
            remain = 4;
            ret = ch & 0x03;
        } else if((ch & 0xFE) == 0xFC) {
            min = 0x4000000;
            remain = 5;
            ret = ch & 0x01;
        } else {
            goto ILSEQ;
        }

        while(remain--) {
            if(!--size) goto ILSEQ;
            ch = *src++;
            if(used) (*used)++;
            if((ch & 0xC0) != 0x80) goto ILSEQ;
            ret <<= 6;
            ret |= ch & 0x3F;
        }

        if(ret < min) goto ILSEQ;

        return ret;
    }
    return ch;

  ILSEQ:
    /* advance pointer to next valid char boundary */
    while(1) {
        if(!*src || !size) break;
        if((*src & 0xC0) == 0x80) break;
        ++src;
        --size;
        if(used) (*used)++;
    }

    return ilseq;
}



wchar_t*
utf8_decode(wchar_t* dest, size_t size, const char* src)
{
    struct utf8_decode_state ctx;
    memset(&ctx, 0, sizeof(ctx));
    ctx.rd = src;
    ctx.rd_remain = -1;
    ctx.wr = dest;
    ctx.wr_size = size;

    if(!utf8_decoder(&ctx)) return 0;
    if(*ctx.rd) {
        errno = ENOMEM;
        return 0;
    }

    return dest;
}



wchar_t*
utf8_decode2(wchar_t* dest, size_t size, size_t* written, const char* src,
    size_t amt)
{
    struct utf8_decode_state ctx;
    memset(&ctx, 0, sizeof(ctx));
    ctx.rd = src;
    ctx.rd_remain = amt;
    ctx.wr = dest;
    ctx.wr_size = size;

    if(!utf8_decoder(&ctx)) return 0;
    if(ctx.rd_remain || !ctx.complete) {
        errno = ENOMEM;
        return 0;
    }
    if(written) *written = ctx.written;

    return dest;
}



wchar_t*
utf8_decode_force(wchar_t* dest, size_t size, const char* src)
{
    struct utf8_decode_state ctx;
    memset(&ctx, 0, sizeof(ctx));
    ctx.rd = src;
    ctx.rd_remain = -1;
    ctx.wr = dest;
    ctx.wr_size = size;
    ctx.error_callback = utf8_decode_error_callback_replace;

    if(!utf8_decoder(&ctx)) return 0;
    if(*ctx.rd) {
        errno = ENOMEM;
        return 0;
    }

    return dest;
}



wchar_t*
utf8_decode_force2(wchar_t* dest, size_t size, size_t* written, const char* src,
    size_t amt)
{
    struct utf8_decode_state ctx;
    memset(&ctx, 0, sizeof(ctx));
    ctx.rd = src;
    ctx.rd_remain = amt;
    ctx.wr = dest;
    ctx.wr_size = size;
    ctx.error_callback = utf8_decode_error_callback_replace;

    if(!utf8_decoder(&ctx)) return 0;
    if(written) *written = ctx.written;
    return dest;
}



/* options for text editors
kate: replace-trailing-space-save true; space-indent true; tab-width 4;
vim: expandtab:ts=4:sw=4:syntax=c.doxygen
*/
