/* libutf8/src/libutf8/300_encode.h
 *
 *  (c)2006-2009, Laurence Withers, <l@lwithers.me.uk>.
 *  Released under the GNU GPLv3. See file COPYING or
 *  http://www.gnu.org/copyleft/gpl.html for details.
*/



/*! \defgroup encode UTF-8 encoding routines

The functions in this module allow encoding of UTF-8 characters. Errors are reported through
\c errno, with the following errors being of particular interest:

\li \c EINVAL - invalid argument to function
\li \c EILSEQ - illegal source character (see utf8_isvalid())
\li \c ENOMEM - not enough space in destination buffer

*/
/*!@{*/



/*! \brief Encode a single character into UTF-8.

\param dest The destination buffer.
\param amt Number of bytes in destination buffer.
\param ch Character to encode.
\returns Pointer to next byte of buffer to use.
\retval 0 on error (see \c errno).

This function will encode a single character into UTF-8. It returns a pointer to the end of the
character (i.e. the next position in the buffer you want to write to).

On error, it sets \c errno (to \c EINVAL, if \a dest is null or \a amt is less than 1, \c EILSEQ
if \a ch is not valid; or \c ENOMEM if the result would not fit into
\a amt bytes) and returns 0.

*/
char* utf8_encode_char(char* dest, size_t amt, wchar_t ch);



/*! \brief Encode a single character into UTF-8, forcing replacement of invalid characters.

\param dest The destination buffer.
\param amt Number of bytes in destination buffer.
\param ch Character to encode.
\param ilseq If \a ch is not a legal character, then this is encoded instead.
\returns Pointer to next byte of buffer to use.
\retval 0 on error (see \c errno).

This function will encode a single character into UTF-8. It returns a pointer to the end of the
character (i.e. the next position in the buffer you want to write to). If the source character \a ch
is not a valid code point, it will instead encode the character \a ilseq.

On error, it sets \c errno (to \c EINVAL, if \a dest is null or \a amt is less than 1; \c EILSEQ
if \a ilseq is not valid; or \c ENOMEM if the result would not fit into
\a amt bytes) and returns 0.

*/
char* utf8_encode_char_force(char* dest, size_t amt, wchar_t ch, wchar_t ilseq);



/*! \brief Encode a null-terminated string into UTF-8.

\param dest The destination buffer.
\param amt Number of bytes in the destination buffer.
\param src Null-terminated source string.
\returns Pointer to destination buffer.
\retval 0 on error (see \c errno).

This function encodes a null-terminated Unicode string into the destination buffer. It returns a
pointer to the destination buffer on success, and 0 on error. If there is not enough space in the
buffer, or an illegal character is encountered somewhere in the sequence, it will fail.

*/
char* utf8_encode(char* dest, size_t amt, const wchar_t* src);



/*! \brief Encode a fixed-size string into UTF-8.

\param dest The destination buffer.
\param amt Number of bytes in the destination buffer.
\param written Set to number of bytes written on success (excluding NUL).
\param src Pointer to source string.
\param inamt Number of characters to encode.
\returns Pointer to destination buffer.
\retval 0 on error (see \c errno).

This function encodes a Unicode string (possibly containing ASCII NUL) into the destination buffer.
It returns a pointer to the destination buffer on success, and 0 on error. If there is not enough
space in the buffer, or an illegal character is encountered somewhere in the sequence, it will fail.
The destination will be null-terminated.

*/
char* utf8_encode2(char* dest, size_t amt, size_t* written, const wchar_t* src, size_t inamt);



/*! \brief Encode a null-terminated string into UTF-8, ignoring errors.

\param dest The destination buffer.
\param amt Number of bytes in the destination buffer.
\param src Null-terminated source string.
\returns Pointer to destination buffer.
\returns 0 if arguments are invalid.

This function will encode a null-terminated Unicode string into the destination buffer, making a
best-effort in the case of failures. If there is not enough memory, the destination string will be
truncated (but still null-terminated). If an illegal source character is encountered, it is replaced
with the Unicode replacement character U+FFFD. The function can only fail if one of the arguments is
invalid.

*/
char* utf8_encode_force(char* dest, size_t amt, const wchar_t* src);



/*! \brief Encode a fixed-size string into UTF-8, ignoring errors.

\param dest The destination buffer.
\param amt Number of bytes in the destination buffer.
\param written Set to number of bytes written on success (excluding NUL).
\param src Null-terminated source string.
\param inamt Number of characters to encode.
\returns Pointer to destination buffer.
\returns 0 if arguments are invalid.

This function will encode a Unicode string (possibly containing ASCII NUL) into the destination
buffer, making a best-effort in the case of failures. If there is not enough memory, the destination
string will be truncated (but still null-terminated). If an illegal source character is encountered,
it is replaced with the Unicode replacement character U+FFFD. The function can only fail if one of
the arguments is invalid.

*/
char* utf8_encode_force2(char* dest, size_t amt, size_t* written, const wchar_t* src, size_t inamt);



/*!@}*/
/* options for text editors
kate: replace-trailing-space-save true; space-indent true; tab-width 4;
vim: expandtab:ts=4:sw=4:syntax=c.doxygen
*/
