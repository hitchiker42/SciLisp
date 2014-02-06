/* libutf8/src/libutf8/500_encode_state.c
 *
 *  (c)2006-2009, Laurence Withers, <l@lwithers.me.uk>.
 *  Released under the GNU GPLv3. See file COPYING or
 *  http://www.gnu.org/copyleft/gpl.html for details.
*/



struct utf8_encode_state*
utf8_encoder(struct utf8_encode_state* state)
{
    char* wr, * ret, * endp;
    wchar_t ch;
    enum utf8_encode_error_action error_action;
    int reencoding;

    if(!state || !state->rd || !state->wr || state->wr_size < 7) {
        errno = EINVAL;
        return 0;
    }

    wr = state->wr;
    endp = wr + state->wr_size - 1;

    state->written = 0;
    while(state->rd_remain) {
        ch = *state->rd;
        if(!ch && state->rd_remain < 0) break;

        reencoding = 0;
      reencode:
        ret = utf8_encode_char(wr, endp - wr, ch);
        if(!ret) {
            if(errno == ENOMEM) break;
            if(!state->error_callback || reencoding) {
                errno = EILSEQ;
                return 0;
            }
            error_action = state->error_callback(state, &ch);
            switch(error_action) {
            case utf8_encode_error_action_abort:
                errno = EILSEQ;
                return 0;

            case utf8_encode_error_action_replace:
                reencoding = 1;
                goto reencode;

            case utf8_encode_error_action_skip:
                ret = wr;
                break;
            }
        }
        if(state->rd_remain > 0) state->rd_remain--;
        ++state->rd;
        ++state->char_offset;
        if(ch == 0x0A || ch == 0x2028) {
            ++state->line;
            state->col = 0;
        } else {
            ++state->col;
        }
        state->written += ret - wr;
        wr = ret;
        if(wr == endp) break;
    }
    *wr = 0;
    return state;
}



enum utf8_encode_error_action
utf8_encode_error_callback_replace(const struct utf8_encode_state* state,
    wchar_t* newch)
{
    (void)state;
    *newch = 0xFFFD;
    return utf8_encode_error_action_replace;
}



enum utf8_encode_error_action
    utf8_encode_error_callback_skip(const struct utf8_encode_state* state,
    wchar_t* newch)
{
    (void)state;
    (void)newch;
    return utf8_encode_error_action_skip;
}



/* options for text editors
kate: replace-trailing-space-save true; space-indent true; tab-width 4;
vim: expandtab:ts=4:sw=4:syntax=c.doxygen
*/
