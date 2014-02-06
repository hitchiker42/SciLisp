/* libutf8/src/libutf8/500_encode_state.h
 *
 *  (c)2006-2009, Laurence Withers, <l@lwithers.me.uk>.
 *  Released under the GNU GPLv3. See file COPYING or
 *  http://www.gnu.org/copyleft/gpl.html for details.
*/



/*! \defgroup encode_state UTF-8 stateful encoder

This UTF-8 encoder uses a structure to maintain state information between calls. This means that
you can feed it a stream of data as it comes in without needing to store the entire source in a
buffer.

Errors (i.e. illegal source chars; see utf8_isvalid()) are handled by providing a callback function
(several of which are provided by the library). The callback function has the option of aborting
the conversion, substituting a replacement character, or simply skipping the illegal source
character.

*/
/*!@{*/



/* opaque type */
struct utf8_encode_state;



/*! \brief Action to be taken after error callback.

These are the possible actions that can be undertaken after a stateful encoding operation has
encountered an error (illegal source char). These actions are specified by the error callback
function's return value.

*/
enum utf8_encode_error_action {
    /*! \brief Abort the conversion, returning EILSEQ. */
    utf8_encode_error_action_abort,

    /*! \brief Skip the illegal byte sequence. */
    utf8_encode_error_action_skip,

    /*! \brief Discard the illegal byte sequence and enter a replacement char. */
    utf8_encode_error_action_replace
};




/*! \brief Error callback type.

\param state The encoder state information.
\param[out] newch If \a utf8_encode_error_action_replace is returned, this is set to the
    character that should be substituted instead of the illegal source character.

This function is called whenever an error occurs. It can examine \a state (and specifically
\a *state.rd) to determine the illegal source character. It can choose to skip the character, replace
it with something else, or abort the conversion entirely.

*/
typedef enum utf8_encode_error_action (*utf8_encode_error_callback)(
    const struct utf8_encode_state* state, wchar_t* newch);

/*! \brief Standard error callback: use replacement char 0xFFFD. */
enum utf8_encode_error_action utf8_encode_error_callback_replace(
    const struct utf8_encode_state* state, wchar_t* newch);

/*! \brief Standard error callback: skip invalid chars. */
enum utf8_encode_error_action utf8_encode_error_callback_skip(
    const struct utf8_encode_state* state, wchar_t* newch);



/*! \brief State structure used to encode Unicode into UTF-8.

This structure is used to encode an arbitrary Unicode string into UTF-8. To set it up, first call
\a memset to clear the structure to zero. You will then
want to set \a rd to point to your input string, with \a rd_remain the number of bytes to encode
(you can set it to a negative number if \a rd is null-terminated and you want to encode the whole
thing). You will also want to tell it where to write to (\a wr) and how much space there is in that
buffer (\a wr_size).

To deal with errors (illegal input chars), you can provide a callback function \a error_callback.
An arbitrary \a data pointer is provided in case you wish to associate some object with the encode
operation. Passing a null pointer for \a error_callback is a valid way of indicating you do not
wish to attempt to correct errors.

You can examine the \a line and \a col variables to get the line / column of the input data at which
the decoder is currently operating. These variables aren't perfect, as they can be
affected by errors and limitations (only 0x0A and 0x2028 are recognised as line end chars, and the
effect of tabs is ignored). \a char_offset represents the offset, in complete characters, from the
start of the stream, and should always be accurate.

*/
struct utf8_encode_state {
    /*! \brief Current read position. */
    const wchar_t* rd;

    /*! \brief Number of chars remaining (-ve means to scan for null char). */
    int rd_remain;

    /*! \brief Callback function used to handle illegal source characters. */
    utf8_encode_error_callback error_callback;

    /*! \brief Output buffer. */
    char* wr;

    /*! \brief Output buffer size. */
    size_t wr_size;

    /*! \brief Number of bytes written during last call. */
    size_t written;

    /*! \brief Arbitrary pointer (useful for \a error_callback). */
    void* data;

    /*! \brief Current line (starting from 0). */
    int line;

    /*! \brief Current column (starting from 0). */
    int col;

    /*! \brief Character offset from start of data (starting from 0). */
    int char_offset;
};



/*! \brief Encode an arbitrary Unicode string.

\param state The encoder state information.
\retval state on success.
\retval 0 on error (see \c errno).

This function is used to encode some arbitrary Unicode string into UTF-8. It uses a state-storage
structure which allows you to perform the encoding in multiple passes (e.g. if you are encoding
an arbitrary string and outputting it, you will want to use a fixed size buffer and this might
be smaller than required).

In each pass of the function, \a rd and \a rd_remain will be updated to record the current reading
position and the number of bytes left to encode. If the function completes this pass, \a rd_remain
will be zero (but if you are converting a null-terminated string, you will need to check for \a *rd
to be zero instead).

After each call, \a wr will be unchanged but \a written will contain the number of bytes written
(excluding a terminating null, which is always written). If you do not want to overwrite this data
on the next call, you will have to update \a wr and \a wr_size.

If \a state is null, or not filled out properly (no source data or destination buffer not at least 7
bytes large), then no conversion will be performed and \a EINVAL will be stored in \a errno. If an
illegal source character is encountered, and the error callback is 0, aborts the process or tries
to replace the char with another illegal code point, then \a EILSEQ will be stored in \a errno. On
error, 0 will be returned.

*/
struct utf8_encode_state* utf8_encoder(struct utf8_encode_state* state);



/*!@}*/
/* options for text editors
kate: replace-trailing-space-save true; space-indent true; tab-width 4;
vim: expandtab:ts=4:sw=4:syntax=c.doxygen
*/
