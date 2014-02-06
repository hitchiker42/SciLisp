/* libutf8/src/libutf8/400_decode_state.h
 *
 *  (c)2006-2009, Laurence Withers, <l@lwithers.me.uk>.
 *  Released under the GNU GPLv3. See file COPYING or
 *  http://www.gnu.org/copyleft/gpl.html for details.
*/



/*! \defgroup decode_ctx UTF-8 stateful decoder

This UTF-8 decoder uses a structure to maintain state information between calls. This means that
you can feed it a stream of data as it comes in without needing to store the entire document in a
buffer. It correctly copes with the currently-available data ending on a non-character boundary.

Errors are handled by providing a callback function (several of which are provided by the library).
The callback function has the option of aborting the conversion, substituting a replacement
character, or simply skipping the illegal byte sequence.

*/
/*!@{*/



/* opaque type */
struct utf8_decode_state;



/*! \brief Types of decoder error.

These are the types of error that can be encountered by the decoder. This allows slightly more
information than is provided by setting \a errno to \c EILSEQ. The type of error will be passed
to the callback function.

*/
enum utf8_decode_error {
    /*! \brief Lone continuation char encountered when start char expected. */
    utf8_decode_error_lone_cchar,

    /*! \brief Non-continuation char encountered within multibyte sequence. */
    utf8_decode_error_not_cchar,

    /*! \brief Invalid start char (not ASCII). */
    utf8_decode_error_not_schar,

    /*! \brief Overlong byte sequence. */
    utf8_decode_error_overlong,

    /*! \brief Illegal code positions (UTF-16 surrogates or 0xFFFE,0xFFFF). */
    utf8_decode_error_illegal_cp
};



/*! \brief Action to be taken after error callback.

These are the possible actions that can be undertaken after a stateful decode has encountered an
error. These actions are specified by the error callback function's return value.

*/
enum utf8_decode_error_action {
    /*! \brief Abort the conversion, returning EILSEQ. */
    utf8_decode_error_action_abort,

    /*! \brief Skip the illegal byte sequence. */
    utf8_decode_error_action_skip,

    /*! \brief Discard the illegal byte sequence and enter a replacement char. */
    utf8_decode_error_action_replace
};



/*! \brief Error callback type.

\param state The state-storage structure.
\param error The error type.
\param[out] newch If utf8_decode_error_action_replace is returned, then set this to the value of
    the character you wish to replace with (\c 0xFFFD is recommended).
\returns A value specifying what action to undertake as a result of the callback.

This callback determines the action of the UTF-8 stateful decoder on encountering an illegal byte
sequence. It can choose to abort the conversion, skip the illegal sequence, or replace the illegal
sequence with an arbitrary character.

*/
typedef enum utf8_decode_error_action(*utf8_decode_error_callback)(
    const struct utf8_decode_state* state, enum utf8_decode_error error, wchar_t* newch);



/*! \brief State structure used to decode UTF-8 into Unicode.

This structure is used to decode arbitrary chunks of UTF-8 data into Unicode. It can deal with
partial data streams (even if they are cut-off mid-character).

Before calling utf8_decoder, you must set up the object appropriately. The first step is to use
\a memset to initialise everything to 0. Then you need to fill out the read and write pointers, and
possibly set up the error callback.

To use it, you set \a rd to point to your input data and \a rd_remain to the amount you have. If
\a rd_remain is negative, the input data is assumed to be null-terminated; otherwise, it is taken
as the number of bytes remaining at the input. These are updated after each call, so simply check
if \a rd_remain is 0 (or \a *rd is 0 in the case of a null-terminated string).

You must also set \a wr (pointer to destination buffer) and \a wr_size (number of characters that
can be written there), and \a written is set for you (it is the number of characters written per
call but excluding the terminating NUL). This implies that the buffer must have space for at least
two characters. You can change \a wr and \a wr_size at any time, but if you leave them the same the
data will be overwritten on each call.

If you wish to do error recovery, set \a error_callback and possibly \a data.

You can examine the \a line and \a col variables to get the line / column of the input data at which
the decoder is currently operating. \a char_offset and \a byte_offset represent the offset, in
complete characters or bytes, from the start of the stream. With the exception of \a byte_offset,
these variables aren't perfect, as they can be affected by errors and limitations (only 0x0A and
0x2028 are recognised as line end chars, and the effect of tabs is ignored).

*/
struct utf8_decode_state {
    /*! \brief 0 if we are part-way through a multi-byte character. */
    int complete;

    /*! \brief Data to read (current read position). */
    const char* rd;

    /*! \brief Number of bytes remaining (current). */
    int rd_remain;

    /*! \brief Internal state; initialise to 0, don't change. */
    int state;

    /*! \brief Error callback (may be 0). */
    utf8_decode_error_callback error_callback;

    /*! \brief Pointer to output buffer. */
    wchar_t* wr;

    /*! \brief Number of characters that can be written. */
    size_t wr_size;

    /*! \brief Number of characters written on last call. */
    size_t written;

    /*! \brief Arbitrary data pointer for \a error_callback. */
    void* data;

    /*! \brief Current line (starting from 0). */
    int line;

    /*! \brief Current column (starting from 0). */
    int col;

    /*! \brief Character offset from start of data (starting from 0). */
    int char_offset;

    /*! \brief Byte offset from start of data (starting from 0). */
    int byte_offset;

    /*! \brief Don't use this. */
    wchar_t statech;
    /*! \brief Don't use this. */
    wchar_t minch;
};



/*! \brief Decode an arbitrary chunk of a UTF-8 byte stream.

\param state The state-storage structure.
\retval ctx on success.
\retval 0 on error (see \a errno).

This function is used to do multi-pass decoding of arbitrary UTF-8 byte streams. Each call will
update \a state.rd, \a state.rd_remain and \a state.written. \a state.complete is \c true if, on consumption
of all the data, we are not inside a multibyte character.

Should an error occur, \a state.error_callback is called (if it is not 0). If it is 0, or it returns
utf8_decode_error_action_abort, then the conversion will be aborted and the object set into
an error state. \a errno will be set to \c EILSEQ. Once the object is in an error state, there is
no way to recover short of completely clearing it and starting with fresh data. Continuing to call
this function with an invalid object will result in \c EINVAL.

*/
struct utf8_decode_state* utf8_decoder(struct utf8_decode_state* state);



/*! \brief Standard error callback: use replacement char 0xFFFD. */
enum utf8_decode_error_action utf8_decode_error_callback_replace(
    const struct utf8_decode_state* ctx, enum utf8_decode_error error, wchar_t* newch);

/*! \brief Standard error callback: skip invalid chars. */
enum utf8_decode_error_action utf8_decode_error_callback_skip(
    const struct utf8_decode_state* ctx, enum utf8_decode_error error, wchar_t* newch);
