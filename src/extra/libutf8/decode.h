/* libutf8/src/libutf8/200_decode.h
 *
 *  (c)2006-2009, Laurence Withers, <l@lwithers.me.uk>.
 *  Released under the GNU GPLv3. See file COPYING or
 *  http://www.gnu.org/copyleft/gpl.html for details.
*/



/*! \defgroup decode UTF-8 decoding routines

These routines decode UTF-8 data into C's wide character type \c wchar_t. Errors are reported
through \c errno, with the following errors being of particular interest:

\li \c EINVAL - invalid argument to function
\li \c EILSEQ - illegal encoding (i.e. not UTF-8 or encoding error)
\li \c ENOMEM - not enough space in destination buffer

As a special case, functions which return a character may return the \c wchar_t representation of
-1 to signify an error. This wording is used to take into account the fact that the \c wchar_t type
could be unsigned.

*/
/*!@{*/



/*! \brief Decode a character.

\param src Pointer to start of source data.
\param used If not null, set to the number of bytes used.
\retval (wchar_t)(-1) on error (see \c errno).
\returns Decoded character.

Decodes a single character, returning the \c wchar_t representation of -1 on error and setting
\c errno appropriately. If \a used is not NULL, it is set to the number of characters used. Passing
a null pointer for \a src will result in \c EINVAL. If the UTF-8 byte sequence is corrupt, \a errno
will be set to \c EILSEQ.

\warning Only use this function if you are sure it cannot read past the end of your buffer. See
    utf8_decode_char2() for a safe version.

*/
wchar_t utf8_decode_char(const char* src, size_t* used);



/*! \brief Decode a character, discarding illegal sequences.

\param src Pointer to start of source data.
\param used If not null, set to the number of bytes used.
\param ilseq This value is returned if the UTF-8 byte sequence is invalid. Recommended is the
    Unicode replacement character, \c 0xFFFD.
\retval (wchar_t)(-1) on error (see \c errno).
\retval ilseq If an illegal sequence is encountered.
\returns Decoded character.
\post \a *used will be set to the number of bytes consumed.

Decodes a single character, returning the \c wchar_t representation of -1 on error and setting
\c errno appropriately. If \a used is not NULL, it is set to the number of characters used. Passing
a null pointer for \a src will result in \c EINVAL. If the UTF-8 byte sequence is corrupt, \a ilseq
will be returned and the buffer advanced to the next valid character. This means the function can
only fail if you pass it an invalid \a src pointer.

\warning Only use this function if you are sure it cannot read past the end of your buffer. See
    utf8_decode_char2_force() for a safe version.

*/
wchar_t utf8_decode_char_force(const char* src, size_t* used, wchar_t ilseq);



/*! \brief Decode a character, given source buffer size.

\param src Pointer to start of source data.
\param size Size of source data in bytes.
\param used If not null, set to the number of bytes used.
\retval (wchar_t)(-1) on error (see \c errno).
\returns Decoded character.

Decodes a single character, returning the \c wchar_t representation of -1 on error and setting
\c errno appropriately. If \a used is not NULL, it is set to the number of characters used.

*/
wchar_t utf8_decode_char2(const char* src, size_t size, size_t* used);



/*! \brief Decode a character, discarding illegal sequences and given source buffer size.

\param src Pointer to start of source data.
\param size Size of source data in bytes.
\param used If not null, set to the number of bytes used.
\param ilseq This value is returned if the UTF-8 byte sequence is invalid. Recommended is the
    Unicode replacement character, \c 0xFFFD.
\retval (wchar_t)(-1) on error (see \c errno).
\retval ilseq If an illegal sequence is encountered.
\returns Decoded character.
\post \a *used will be set to the number of bytes consumed.

Decodes a single character, returning the \c wchar_t representation of -1 on error and setting
\c errno appropriately. If \a used is not NULL, it is set to the number of characters used. Passing
a null pointer for \a src will result in \c EINVAL. If the UTF-8 byte sequence is corrupt, \a ilseq
will be returned and the buffer advanced to the next valid character. This means the function can
only fail if you pass it an invalid \a src pointer, or a \a size of 0.

*/
wchar_t utf8_decode_char2_force(const char* src, size_t size, size_t* used, wchar_t ilseq);



/*! \brief Decode a null-terminated string.

\param dest The output destination.
\param size The number of characters that can be stored in \a dest.
\param src Pointer to the null-terminated source data.
\returns Pointer to the output destination.
\retval 0 on error (see \c errno).

This function will attempt to decode a null-terminated UTF-8 string. It returns 0 on error and sets
\c errno appropriately.

*/
wchar_t* utf8_decode(wchar_t* dest, size_t size, const char* src);



/*! \brief Decode a fixed-size string.

\param dest The output destination.
\param size The number of characters that can be stored in \a dest.
\param written Set to the number of bytes written (excluding NUL).
\param src Pointer to the null-terminated source data.
\param amt Number of bytes to decode.
\returns Pointer to the output destination.
\retval 0 on error (see \c errno).

This function will attempt to decode a fixed-size UTF-8 string. It returns 0 on error and sets
\c errno appropriately. It will happily transcode ASCII NUL characters. If \a written is not null,
it is set to the number of characters written excluding the terminating NUL. This function always
produces null-terminated strings.

*/
wchar_t* utf8_decode2(wchar_t* dest, size_t size, size_t* written, const char* src, size_t amt);



/*! \brief Decode a null-terminated string, ignoring errors.

\param dest The output destination.
\param size The number of characters that can be stored in \a dest.
\param src Pointer to the null-terminated source data.
\returns Pointer to the output destination.
\retval 0 on error (see \c errno).

This function will attempt to decode a null-terminated UTF-8 string. It returns 0 on error and sets
\c errno appropriately.

This function will truncate the output if there is not enough space and will skip characters it
cannot decode. It can only fail if you pass it invalid parameters.

*/
wchar_t* utf8_decode_force(wchar_t* dest, size_t size, const char* src);



/*! \brief Decode a fixed-size string, ignoring errors.

\param dest The output destination.
\param size The number of characters that can be stored in \a dest.
\param written Set to the number of bytes written (excluding NUL).
\param src Pointer to the null-terminated source data.
\param amt Number of bytes to decode.
\returns Pointer to the output destination.
\retval 0 on error (see \c errno).

This function will attempt to decode a fixed-size UTF-8 string. It returns 0 on error and sets
\c errno appropriately. It will happily transcode ASCII NUL characters. If \a written is not null,
it is set to the number of characters written excluding the terminating NUL. This function always
produces null-terminated strings.

This function will truncate the output if there is not enough space and will skip characters it
cannot decode. It can only fail if you pass it invalid parameters.

*/
wchar_t* utf8_decode_force2(wchar_t* dest, size_t size, size_t* written, const char* src, size_t amt);



/*!@}*/
/* options for text editors
kate: replace-trailing-space-save true; space-indent true; tab-width 4;
vim: expandtab:ts=4:sw=4:syntax=c.doxygen
*/
