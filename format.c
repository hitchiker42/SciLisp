//verbatim emacs code for now, just here as a placeholder/guide
DEFUN ("format", Fformat, Sformat, 1, MANY, 0,
       doc: /* Format a string out of a format-string and arguments.
The first argument is a format control string.
The other arguments are substituted into it to make the result, a string.

The format control string may contain %-sequences meaning to substitute
the next available argument:

%s means print a string argument.  Actually, prints any object, with `princ'.
%d means print as number in decimal (%o octal, %x hex).
%X is like %x, but uses upper case.
%e means print a number in exponential notation.
%f means print a number in decimal-point notation.
%g means print a number in exponential notation
  or decimal-point notation, whichever uses fewer characters.
%c means print a number as a single character.
%S means print any object as an s-expression (using `prin1').

The argument used for %d, %o, %x, %e, %f, %g or %c must be a number.
Use %% to put a single % into the output.

A %-sequence may contain optional flag, width, and precision
specifiers, as follows:

  %<flags><width><precision>character

where flags is [+ #-0]+, width is [0-9]+, and precision is .[0-9]+

The + flag character inserts a + before any positive number, while a
space inserts a space before any positive number; these flags only
affect %d, %e, %f, and %g sequences, and the + flag takes precedence.
The - and 0 flags affect the width specifier, as described below.

The # flag means to use an alternate display form for %o, %x, %X, %e,
%f, and %g sequences: for %o, it ensures that the result begins with
\"0\"; for %x and %X, it prefixes the result with \"0x\" or \"0X\";
for %e, %f, and %g, it causes a decimal point to be included even if
the precision is zero.

The width specifier supplies a lower limit for the length of the
printed representation.  The padding, if any, normally goes on the
left, but it goes on the right if the - flag is present.  The padding
character is normally a space, but it is 0 if the 0 flag is present.
The 0 flag is ignored if the - flag is present, or the format sequence
is something other than %d, %e, %f, and %g.

For %e, %f, and %g sequences, the number after the "." in the
precision specifier says how many decimal places to show; if zero, the
decimal point itself is omitted.  For %s and %S, the precision
specifier truncates the string to the given width.

usage: (format STRING &rest OBJECTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t n;		/* The number of the next arg to substitute */
  char initial_buffer[4000];
  char *buf = initial_buffer;
  ptrdiff_t bufsize = sizeof initial_buffer;
  ptrdiff_t max_bufsize = STRING_BYTES_BOUND + 1;
  char *p;
  ptrdiff_t buf_save_value_index IF_LINT (= 0);
  char *format, *end, *format_start;
  ptrdiff_t formatlen, nchars;
  /* True if the format is multibyte.  */
  bool multibyte_format = 0;
  /* True if the output should be a multibyte string,
     which is true if any of the inputs is one.  */
  bool multibyte = 0;
  /* When we make a multibyte string, we must pay attention to the
     byte combining problem, i.e., a byte may be combined with a
     multibyte character of the previous string.  This flag tells if we
     must consider such a situation or not.  */
  bool maybe_combine_byte;
  Lisp_Object val;
  bool arg_intervals = 0;
  USE_SAFE_ALLOCA;

  /* discarded[I] is 1 if byte I of the format
     string was not copied into the output.
     It is 2 if byte I was not the first byte of its character.  */
  char *discarded;

  /* Each element records, for one argument,
     the start and end bytepos in the output string,
     whether the argument has been converted to string (e.g., due to "%S"),
     and whether the argument is a string with intervals.
     info[0] is unused.  Unused elements have -1 for start.  */
  struct info
  {
    ptrdiff_t start, end;
    unsigned converted_to_string : 1;
    unsigned intervals : 1;
  } *info = 0;

  /* It should not be necessary to GCPRO ARGS, because
     the caller in the interpreter should take care of that.  */

  CHECK_STRING (args[0]);
  format_start = SSDATA (args[0]);
  formatlen = SBYTES (args[0]);

  /* Allocate the info and discarded tables.  */
  {
    ptrdiff_t i;
    if ((SIZE_MAX - formatlen) / sizeof (struct info) <= nargs)
      memory_full (SIZE_MAX);
    info = SAFE_ALLOCA ((nargs + 1) * sizeof *info + formatlen);
    discarded = (char *) &info[nargs + 1];
    for (i = 0; i < nargs + 1; i++)
      {
	info[i].start = -1;
	info[i].intervals = info[i].converted_to_string = 0;
      }
    memset (discarded, 0, formatlen);
  }

  /* Try to determine whether the result should be multibyte.
     This is not always right; sometimes the result needs to be multibyte
     because of an object that we will pass through prin1,
     and in that case, we won't know it here.  */
  multibyte_format = STRING_MULTIBYTE (args[0]);
  multibyte = multibyte_format;
  for (n = 1; !multibyte && n < nargs; n++)
    if (STRINGP (args[n]) && STRING_MULTIBYTE (args[n]))
      multibyte = 1;

  /* If we start out planning a unibyte result,
     then discover it has to be multibyte, we jump back to retry.  */
 retry:

  p = buf;
  nchars = 0;
  n = 0;

  /* Scan the format and store result in BUF.  */
  format = format_start;
  end = format + formatlen;
  maybe_combine_byte = 0;

  while (format != end)
    {
      /* The values of N and FORMAT when the loop body is entered.  */
      ptrdiff_t n0 = n;
      char *format0 = format;

      /* Bytes needed to represent the output of this conversion.  */
      ptrdiff_t convbytes;

      if (*format == '%')
	{
	  /* General format specifications look like

	     '%' [flags] [field-width] [precision] format

	     where

	     flags ::= [-+0# ]+
	     field-width ::= [0-9]+
	     precision ::= '.' [0-9]*

	     If a field-width is specified, it specifies to which width
	     the output should be padded with blanks, if the output
	     string is shorter than field-width.

	     If precision is specified, it specifies the number of
	     digits to print after the '.' for floats, or the max.
	     number of chars to print from a string.  */

	  bool minus_flag = 0;
	  bool  plus_flag = 0;
	  bool space_flag = 0;
	  bool sharp_flag = 0;
	  bool  zero_flag = 0;
	  ptrdiff_t field_width;
	  bool precision_given;
	  uintmax_t precision = UINTMAX_MAX;
	  char *num_end;
	  char conversion;

	  while (1)
	    {
	      switch (*++format)
		{
		case '-': minus_flag = 1; continue;
		case '+':  plus_flag = 1; continue;
		case ' ': space_flag = 1; continue;
		case '#': sharp_flag = 1; continue;
		case '0':  zero_flag = 1; continue;
		}
	      break;
	    }

	  /* Ignore flags when sprintf ignores them.  */
	  space_flag &= ~ plus_flag;
	  zero_flag &= ~ minus_flag;

	  {
	    uintmax_t w = strtoumax (format, &num_end, 10);
	    if (max_bufsize <= w)
	      string_overflow ();
	    field_width = w;
	  }
	  precision_given = *num_end == '.';
	  if (precision_given)
	    precision = strtoumax (num_end + 1, &num_end, 10);
	  format = num_end;

	  if (format == end)
	    error ("Format string ends in middle of format specifier");

	  memset (&discarded[format0 - format_start], 1, format - format0);
	  conversion = *format;
	  if (conversion == '%')
	    goto copy_char;
	  discarded[format - format_start] = 1;
	  format++;

	  ++n;
	  if (! (n < nargs))
	    error ("Not enough arguments for format string");

	  /* For 'S', prin1 the argument, and then treat like 's'.
	     For 's', princ any argument that is not a string or
	     symbol.  But don't do this conversion twice, which might
	     happen after retrying.  */
	  if ((conversion == 'S'
	       || (conversion == 's'
		   && ! STRINGP (args[n]) && ! SYMBOLP (args[n]))))
	    {
	      if (! info[n].converted_to_string)
		{
		  Lisp_Object noescape = conversion == 'S' ? Qnil : Qt;
		  args[n] = Fprin1_to_string (args[n], noescape);
		  info[n].converted_to_string = 1;
		  if (STRING_MULTIBYTE (args[n]) && ! multibyte)
		    {
		      multibyte = 1;
		      goto retry;
		    }
		}
	      conversion = 's';
	    }
	  else if (conversion == 'c')
	    {
	      if (FLOATP (args[n]))
		{
		  double d = XFLOAT_DATA (args[n]);
		  args[n] = make_number (FIXNUM_OVERFLOW_P (d) ? -1 : d);
		}

	      if (INTEGERP (args[n]) && ! ASCII_CHAR_P (XINT (args[n])))
		{
		  if (!multibyte)
		    {
		      multibyte = 1;
		      goto retry;
		    }
		  args[n] = Fchar_to_string (args[n]);
		  info[n].converted_to_string = 1;
		}

	      if (info[n].converted_to_string)
		conversion = 's';
	      zero_flag = 0;
	    }

	  if (SYMBOLP (args[n]))
	    {
	      args[n] = SYMBOL_NAME (args[n]);
	      if (STRING_MULTIBYTE (args[n]) && ! multibyte)
		{
		  multibyte = 1;
		  goto retry;
		}
	    }

	  if (conversion == 's')
	    {
	      /* handle case (precision[n] >= 0) */

	      ptrdiff_t width, padding, nbytes;
	      ptrdiff_t nchars_string;

	      ptrdiff_t prec = -1;
	      if (precision_given && precision <= TYPE_MAXIMUM (ptrdiff_t))
		prec = precision;

	      /* lisp_string_width ignores a precision of 0, but GNU
		 libc functions print 0 characters when the precision
		 is 0.  Imitate libc behavior here.  Changing
		 lisp_string_width is the right thing, and will be
		 done, but meanwhile we work with it. */

	      if (prec == 0)
		width = nchars_string = nbytes = 0;
	      else
		{
		  ptrdiff_t nch, nby;
		  width = lisp_string_width (args[n], prec, &nch, &nby);
		  if (prec < 0)
		    {
		      nchars_string = SCHARS (args[n]);
		      nbytes = SBYTES (args[n]);
		    }
		  else
		    {
		      nchars_string = nch;
		      nbytes = nby;
		    }
		}

	      convbytes = nbytes;
	      if (convbytes && multibyte && ! STRING_MULTIBYTE (args[n]))
		convbytes = count_size_as_multibyte (SDATA (args[n]), nbytes);

	      padding = width < field_width ? field_width - width : 0;

	      if (max_bufsize - padding <= convbytes)
		string_overflow ();
	      convbytes += padding;
	      if (convbytes <= buf + bufsize - p)
		{
		  if (! minus_flag)
		    {
		      memset (p, ' ', padding);
		      p += padding;
		      nchars += padding;
		    }

		  if (p > buf
		      && multibyte
		      && !ASCII_BYTE_P (*((unsigned char *) p - 1))
		      && STRING_MULTIBYTE (args[n])
		      && !CHAR_HEAD_P (SREF (args[n], 0)))
		    maybe_combine_byte = 1;

		  p += copy_text (SDATA (args[n]), (unsigned char *) p,
				  nbytes,
				  STRING_MULTIBYTE (args[n]), multibyte);

                  info[n].start = nchars;
		  nchars += nchars_string;
		  info[n].end = nchars;

		  if (minus_flag)
		    {
		      memset (p, ' ', padding);
		      p += padding;
		      nchars += padding;
		    }

		  /* If this argument has text properties, record where
		     in the result string it appears.  */
		  if (string_intervals (args[n]))
		    info[n].intervals = arg_intervals = 1;

		  continue;
		}
	    }
	  else if (! (conversion == 'c' || conversion == 'd'
		      || conversion == 'e' || conversion == 'f'
		      || conversion == 'g' || conversion == 'i'
		      || conversion == 'o' || conversion == 'x'
		      || conversion == 'X'))
	    error ("Invalid format operation %%%c",
		   STRING_CHAR ((unsigned char *) format - 1));
	  else if (! (INTEGERP (args[n]) || FLOATP (args[n])))
	    error ("Format specifier doesn't match argument type");
	  else
	    {
	      enum
	      {
		/* Maximum precision for a %f conversion such that the
		   trailing output digit might be nonzero.  Any precision
		   larger than this will not yield useful information.  */
		USEFUL_PRECISION_MAX =
		  ((1 - DBL_MIN_EXP)
		   * (FLT_RADIX == 2 || FLT_RADIX == 10 ? 1
		      : FLT_RADIX == 16 ? 4
		      : -1)),

		/* Maximum number of bytes generated by any format, if
		   precision is no more than USEFUL_PRECISION_MAX.
		   On all practical hosts, %f is the worst case.  */
		SPRINTF_BUFSIZE =
		  sizeof "-." + (DBL_MAX_10_EXP + 1) + USEFUL_PRECISION_MAX,

		/* Length of pM (that is, of pMd without the
		   trailing "d").  */
		pMlen = sizeof pMd - 2
	      };
	      verify (USEFUL_PRECISION_MAX > 0);

	      int prec;
	      ptrdiff_t padding, sprintf_bytes;
	      uintmax_t excess_precision, numwidth;
	      uintmax_t leading_zeros = 0, trailing_zeros = 0;

	      char sprintf_buf[SPRINTF_BUFSIZE];

	      /* Copy of conversion specification, modified somewhat.
		 At most three flags F can be specified at once.  */
	      char convspec[sizeof "%FFF.*d" + pMlen];

	      /* Avoid undefined behavior in underlying sprintf.  */
	      if (conversion == 'd' || conversion == 'i')
		sharp_flag = 0;

	      /* Create the copy of the conversion specification, with
		 any width and precision removed, with ".*" inserted,
		 and with pM inserted for integer formats.  */
	      {
		char *f = convspec;
		*f++ = '%';
		*f = '-'; f += minus_flag;
		*f = '+'; f +=  plus_flag;
		*f = ' '; f += space_flag;
		*f = '#'; f += sharp_flag;
		*f = '0'; f +=  zero_flag;
                *f++ = '.';
                *f++ = '*';
		if (conversion == 'd' || conversion == 'i'
		    || conversion == 'o' || conversion == 'x'
		    || conversion == 'X')
		  {
		    memcpy (f, pMd, pMlen);
		    f += pMlen;
		    zero_flag &= ~ precision_given;
		  }
		*f++ = conversion;
		*f = '\0';
	      }

	      prec = -1;
	      if (precision_given)
		prec = min (precision, USEFUL_PRECISION_MAX);

	      /* Use sprintf to format this number into sprintf_buf.  Omit
		 padding and excess precision, though, because sprintf limits
		 output length to INT_MAX.

		 There are four types of conversion: double, unsigned
		 char (passed as int), wide signed int, and wide
		 unsigned int.  Treat them separately because the
		 sprintf ABI is sensitive to which type is passed.  Be
		 careful about integer overflow, NaNs, infinities, and
		 conversions; for example, the min and max macros are
		 not suitable here.  */
	      if (conversion == 'e' || conversion == 'f' || conversion == 'g')
		{
		  double x = (INTEGERP (args[n])
			      ? XINT (args[n])
			      : XFLOAT_DATA (args[n]));
		  sprintf_bytes = sprintf (sprintf_buf, convspec, prec, x);
		}
	      else if (conversion == 'c')
		{
		  /* Don't use sprintf here, as it might mishandle prec.  */
		  sprintf_buf[0] = XINT (args[n]);
		  sprintf_bytes = prec != 0;
		}
	      else if (conversion == 'd')
		{
		  /* For float, maybe we should use "%1.0f"
		     instead so it also works for values outside
		     the integer range.  */
		  printmax_t x;
		  if (INTEGERP (args[n]))
		    x = XINT (args[n]);
		  else
		    {
		      double d = XFLOAT_DATA (args[n]);
		      if (d < 0)
			{
			  x = TYPE_MINIMUM (printmax_t);
			  if (x < d)
			    x = d;
			}
		      else
			{
			  x = TYPE_MAXIMUM (printmax_t);
			  if (d < x)
			    x = d;
			}
		    }
		  sprintf_bytes = sprintf (sprintf_buf, convspec, prec, x);
		}
	      else
		{
		  /* Don't sign-extend for octal or hex printing.  */
		  uprintmax_t x;
		  if (INTEGERP (args[n]))
		    x = XUINT (args[n]);
		  else
		    {
		      double d = XFLOAT_DATA (args[n]);
		      if (d < 0)
			x = 0;
		      else
			{
			  x = TYPE_MAXIMUM (uprintmax_t);
			  if (d < x)
			    x = d;
			}
		    }
		  sprintf_bytes = sprintf (sprintf_buf, convspec, prec, x);
		}

	      /* Now the length of the formatted item is known, except it omits
		 padding and excess precision.  Deal with excess precision
		 first.  This happens only when the format specifies
		 ridiculously large precision.  */
	      excess_precision = precision - prec;
	      if (excess_precision)
		{
		  if (conversion == 'e' || conversion == 'f'
		      || conversion == 'g')
		    {
		      if ((conversion == 'g' && ! sharp_flag)
			  || ! ('0' <= sprintf_buf[sprintf_bytes - 1]
				&& sprintf_buf[sprintf_bytes - 1] <= '9'))
			excess_precision = 0;
		      else
			{
			  if (conversion == 'g')
			    {
			      char *dot = strchr (sprintf_buf, '.');
			      if (!dot)
				excess_precision = 0;
			    }
			}
		      trailing_zeros = excess_precision;
		    }
		  else
		    leading_zeros = excess_precision;
		}

	      /* Compute the total bytes needed for this item, including
		 excess precision and padding.  */
	      numwidth = sprintf_bytes + excess_precision;
	      padding = numwidth < field_width ? field_width - numwidth : 0;
	      if (max_bufsize - sprintf_bytes <= excess_precision
		  || max_bufsize - padding <= numwidth)
		string_overflow ();
	      convbytes = numwidth + padding;

	      if (convbytes <= buf + bufsize - p)
		{
		  /* Copy the formatted item from sprintf_buf into buf,
		     inserting padding and excess-precision zeros.  */

                  char *src = sprintf_buf;
		  char src0 = src[0];
		  int exponent_bytes = 0;
		  bool signedp = src0 == '-' || src0 == '+' || src0 == ' ';
		  int significand_bytes;
		  if (zero_flag
		      && ((src[signedp] >= '0' && src[signedp] <= '9')
			  || (src[signedp] >= 'a' && src[signedp] <= 'f')
			  || (src[signedp] >= 'A' && src[signedp] <= 'F')))
		    {
		      leading_zeros += padding;
		      padding = 0;
		    }

		  if (excess_precision
		      && (conversion == 'e' || conversion == 'g'))
		    {
		      char *e = strchr (src, 'e');
		      if (e)
			exponent_bytes = src + sprintf_bytes - e;
		    }

		  if (! minus_flag)
		    {
		      memset (p, ' ', padding);
		      p += padding;
		      nchars += padding;
		    }

		  *p = src0;
		  src += signedp;
		  p += signedp;
		  memset (p, '0', leading_zeros);
		  p += leading_zeros;
		  significand_bytes = sprintf_bytes - signedp - exponent_bytes;
		  memcpy (p, src, significand_bytes);
                  p += significand_bytes;
		  src += significand_bytes;
		  memset (p, '0', trailing_zeros);
		  p += trailing_zeros;
		  memcpy (p, src, exponent_bytes);
		  p += exponent_bytes;

                  info[n].start = nchars;
		  nchars += leading_zeros + sprintf_bytes + trailing_zeros;
		  info[n].end = nchars;

		  if (minus_flag)
		    {
		      memset (p, ' ', padding);
		      p += padding;
		      nchars += padding;
		    }

		  continue;
		}
	    }
	}
      else
      copy_char:
	{
	  /* Copy a single character from format to buf.  */

	  char *src = format;
	  unsigned char str[MAX_MULTIBYTE_LENGTH];

	  if (multibyte_format)
	    {
	      /* Copy a whole multibyte character.  */
	      if (p > buf
		  && !ASCII_BYTE_P (*((unsigned char *) p - 1))
		  && !CHAR_HEAD_P (*format))
		maybe_combine_byte = 1;

	      do
		format++;
	      while (! CHAR_HEAD_P (*format));

	      convbytes = format - src;
	      memset (&discarded[src + 1 - format_start], 2, convbytes - 1);
	    }
	  else
	    {
	      unsigned char uc = *format++;
	      if (! multibyte || ASCII_BYTE_P (uc))
		convbytes = 1;
	      else
		{
		  int c = BYTE8_TO_CHAR (uc);
		  convbytes = CHAR_STRING (c, str);
		  src = (char *) str;
		}
	    }

	  if (convbytes <= buf + bufsize - p)
	    {
	      memcpy (p, src, convbytes);
	      p += convbytes;
	      nchars++;
	      continue;
	    }
	}

      /* There wasn't enough room to store this conversion or single
	 character.  CONVBYTES says how much room is needed.  Allocate
	 enough room (and then some) and do it again.  */
      {
	ptrdiff_t used = p - buf;

	if (max_bufsize - used < convbytes)
	  string_overflow ();
	bufsize = used + convbytes;
	bufsize = bufsize < max_bufsize / 2 ? bufsize * 2 : max_bufsize;

	if (buf == initial_buffer)
	  {
	    buf = xmalloc (bufsize);
	    sa_must_free = 1;
	    buf_save_value_index = SPECPDL_INDEX ();
	    record_unwind_protect_ptr (xfree, buf);
	    memcpy (buf, initial_buffer, used);
	  }
	else
	  {
	    buf = xrealloc (buf, bufsize);
	    set_unwind_protect_ptr (buf_save_value_index, xfree, buf);
	  }

	p = buf + used;
      }

      format = format0;
      n = n0;
    }

  if (bufsize < p - buf)
    emacs_abort ();

  if (maybe_combine_byte)
    nchars = multibyte_chars_in_text ((unsigned char *) buf, p - buf);
  val = make_specified_string (buf, nchars, p - buf, multibyte);

  /* If we allocated BUF with malloc, free it too.  */
  SAFE_FREE ();

  /* If the format string has text properties, or any of the string
     arguments has text properties, set up text properties of the
     result string.  */

  if (string_intervals (args[0]) || arg_intervals)
    {
      Lisp_Object len, new_len, props;
      struct gcpro gcpro1;

      /* Add text properties from the format string.  */
      len = make_number (SCHARS (args[0]));
      props = text_property_list (args[0], make_number (0), len, Qnil);
      GCPRO1 (props);

      if (CONSP (props))
	{
	  ptrdiff_t bytepos = 0, position = 0, translated = 0;
	  ptrdiff_t argn = 1;
	  Lisp_Object list;

	  /* Adjust the bounds of each text property
	     to the proper start and end in the output string.  */

	  /* Put the positions in PROPS in increasing order, so that
	     we can do (effectively) one scan through the position
	     space of the format string.  */
	  props = Fnreverse (props);

	  /* BYTEPOS is the byte position in the format string,
	     POSITION is the untranslated char position in it,
	     TRANSLATED is the translated char position in BUF,
	     and ARGN is the number of the next arg we will come to.  */
	  for (list = props; CONSP (list); list = XCDR (list))
	    {
	      Lisp_Object item;
	      ptrdiff_t pos;

	      item = XCAR (list);

	      /* First adjust the property start position.  */
	      pos = XINT (XCAR (item));

	      /* Advance BYTEPOS, POSITION, TRANSLATED and ARGN
		 up to this position.  */
	      for (; position < pos; bytepos++)
		{
		  if (! discarded[bytepos])
		    position++, translated++;
		  else if (discarded[bytepos] == 1)
		    {
		      position++;
		      if (translated == info[argn].start)
			{
			  translated += info[argn].end - info[argn].start;
			  argn++;
			}
		    }
		}

	      XSETCAR (item, make_number (translated));

	      /* Likewise adjust the property end position.  */
	      pos = XINT (XCAR (XCDR (item)));

	      for (; position < pos; bytepos++)
		{
		  if (! discarded[bytepos])
		    position++, translated++;
		  else if (discarded[bytepos] == 1)
		    {
		      position++;
		      if (translated == info[argn].start)
			{
			  translated += info[argn].end - info[argn].start;
			  argn++;
			}
		    }
		}

	      XSETCAR (XCDR (item), make_number (translated));
	    }

	  add_text_properties_from_list (val, props, make_number (0));
	}

      /* Add text properties from arguments.  */
      if (arg_intervals)
	for (n = 1; n < nargs; ++n)
	  if (info[n].intervals)
	    {
	      len = make_number (SCHARS (args[n]));
	      new_len = make_number (info[n].end - info[n].start);
	      props = text_property_list (args[n], make_number (0), len, Qnil);
	      props = extend_property_ranges (props, new_len);
	      /* If successive arguments have properties, be sure that
		 the value of `composition' property be the copy.  */
	      if (n > 1 && info[n - 1].end)
		make_composition_value_copy (props);
	      add_text_properties_from_list (val, props,
					     make_number (info[n].start));
	    }

      UNGCPRO;
    }

  return val;
}

Lisp_Object
format2 (const char *string1, Lisp_Object arg0, Lisp_Object arg1)
{
  Lisp_Object args[3];
  args[0] = build_string (string1);
  args[1] = arg0;
  args[2] = arg1;
  return Fformat (3, args);
}
