/* ------------------------------------------------------------
   LAMBDA OBJECTS: An interpreted function is a vector made of
	the following components

      #(LAMBDA
	{block-name | NIL}
	{variable-env | NIL}
	{function-env | NIL}
	{block-env | NIL}
	(list of variables declared special)
	Nreq {var}*			; required arguments
	Nopt {var value flag}*		; optional arguments
	{rest-var NIL}			; rest variable
	{T | NIL}			; allow other keys?
	Nkey {key var value flag}*	; keyword arguments
	Naux {var init}			; auxiliary variables
	documentation-string
	list-of-declarations
	{form}*				; body)

   ------------------------------------------------------------ */
/*
 * (si::process-lambda-list lambda-list context)
 *
 * Parses different types of lambda lists. CONTEXT may be MACRO,
 * FTYPE, FUNCTION, METHOD or DESTRUCTURING-BIND, and determines the
 * valid sytax. The output is made of several values:
 *
 * VALUES(0) = (N req1 ... )			; required values
 * VALUES(1) = (N opt1 init1 flag1 ... )	; optional values
 * VALUES(2) = rest-var				; rest-variable, if any
 * VALUES(3) = key-flag				; T if &key was supplied
 * VALUES(4) = (N key1 var1 init1 flag1 ... )	; keyword arguments
 * VALUES(5) = allow-other-keys			; flag &allow-other-keys
 * VALUES(6) = (N aux1 init1 ... )		; auxiliary variables
 *
 * 1°) The prefix "N" is an integer value denoting the number of
 * variables which are declared within this section of the lambda
 * list.
 *
 * 2°) The INIT* arguments are lisp forms which are evaluated when
 * no value is provided.
 *
 * 3°) The FLAG* arguments is the name of a variable which holds a
 * boolean value in case an optional or keyword argument was
 * provided. If it is NIL, no such variable exists.
 */

cl_object
si_process_lambda_list(cl_object org_lambda_list, cl_object context){
#define push(v,l) { cl_object c = *l = CONS(v, *l); l = &ECL_CONS_CDR(c); }
#define assert_var_name(v) \
	if (context == @'function') { \
		unlikely_if (ecl_symbol_type(v) & ecl_stp_constant)	\
			FEillegal_variable_name(v); }

  
        cl_object lists[4] = {ECL_NIL, ECL_NIL, ECL_NIL, ECL_NIL};
        cl_object *reqs = lists, *opts = lists+1, *keys = lists+2, *auxs = lists+3;
	cl_object v, rest = ECL_NIL, lambda_list = org_lambda_list;
	int nreq = 0, nopt = 0, nkey = 0, naux = 0;
	cl_object allow_other_keys = ECL_NIL;
	cl_object key_flag = ECL_NIL;
        enum {  AT_REQUIREDS,
                AT_OPTIONALS,
                AT_REST,
                AT_KEYS,
                AT_OTHER_KEYS,
                AT_AUXS
        } stage = AT_REQUIREDS;

	if (!ECL_LISTP(lambda_list))
		goto ILLEGAL_LAMBDA;
LOOP:
        if (NILP(lambda_list))
                goto OUTPUT;
	if (!ECL_LISTP(lambda_list)) {
		unlikely_if (context == @'function' || context == @'ftype')
			goto ILLEGAL_LAMBDA;
                v = lambda_list;
                lambda_list = ECL_NIL;
                goto REST;
	}
	v = ECL_CONS_CAR(lambda_list);
	lambda_list = ECL_CONS_CDR(lambda_list);
	if (v == @'&optional') {
		unlikely_if (stage >= AT_OPTIONALS)
			goto ILLEGAL_LAMBDA;
		stage = AT_OPTIONALS;
		goto LOOP;
	}
	if (v == @'&rest' || (v == @'&body' && (context == @'si::macro' || context == @'destructuring-bind'))) {
		unlikely_if (ECL_ATOM(lambda_list))
                        goto ILLEGAL_LAMBDA;
		v = ECL_CONS_CAR(lambda_list);
		lambda_list = ECL_CONS_CDR(lambda_list);
REST:		unlikely_if (stage >= AT_REST)
			goto ILLEGAL_LAMBDA;
		stage = AT_REST;
		rest = v;
		goto LOOP;
	}
	if (v == @'&key') {
		unlikely_if (stage >= AT_KEYS)
			goto ILLEGAL_LAMBDA;
		key_flag = ECL_T;
		stage = AT_KEYS;
		goto LOOP;
	}
	if (v == @'&aux') {
		unlikely_if (stage >= AT_AUXS)
			goto ILLEGAL_LAMBDA;
		stage = AT_AUXS;
		goto LOOP;
	}
	if (v == @'&allow-other-keys') {
		allow_other_keys = ECL_T;
		unlikely_if (stage != AT_KEYS)
			goto ILLEGAL_LAMBDA;
		stage = AT_OTHER_KEYS;
		goto LOOP;
	}
	switch (stage) {
	case AT_REQUIREDS:
		nreq++;
                assert_var_name(v);
		push(v, reqs);
		break;
	case AT_OPTIONALS: {
		cl_object spp = ECL_NIL;
		cl_object init = ECL_NIL;
		if (!ECL_ATOM(v) && (context != @'ftype')) {
			cl_object x = v;
                        unlikely_if (!ECL_LISTP(x)) goto ILLEGAL_LAMBDA;
			v = ECL_CONS_CAR(x);
                        x = ECL_CONS_CDR(x);
			if (!Null(x)) {
                                unlikely_if (!ECL_LISTP(x)) goto ILLEGAL_LAMBDA;
				init = ECL_CONS_CAR(x);
                                x = ECL_CONS_CDR(x);
				if (!Null(x)) {
                                        unlikely_if (!ECL_LISTP(x)) goto ILLEGAL_LAMBDA;
					spp = ECL_CONS_CAR(x);
                                        x = ECL_CONS_CDR(x);
                                        if (spp != ECL_NIL) assert_var_name(spp);
					unlikely_if (!Null(x))
						goto ILLEGAL_LAMBDA;
				}
			}
		}
		nopt++;
                assert_var_name(v);
		push(v, opts);
		push(init, opts);
                push(spp, opts);
		break;
        }
	case AT_REST:
		/* If we get here, the user has declared more than one
		 * &rest variable, as in (lambda (&rest x y) ...) */
		goto ILLEGAL_LAMBDA;
	case AT_KEYS: {
		cl_object init = ECL_NIL;
		cl_object spp = ECL_NIL;
                cl_object key;
                if (context == @'ftype') {
                        unlikely_if (ECL_ATOM(v))
                                goto ILLEGAL_LAMBDA;
                        key = ECL_CONS_CAR(v);
                        v = CADR(v);
                        goto KEY_PUSH;
                }
		if (!ECL_ATOM(v)) {
			cl_object x = v;
			v = ECL_CONS_CAR(x);
                        x = ECL_CONS_CDR(x);
			if (!Null(x)) {
                                unlikely_if (!ECL_LISTP(x)) goto ILLEGAL_LAMBDA;
				init = ECL_CONS_CAR(x);
                                x = ECL_CONS_CDR(x);
				if (!Null(x)) {
                                        unlikely_if (!ECL_LISTP(x)) goto ILLEGAL_LAMBDA;
					spp = ECL_CONS_CAR(x);
                                        x = ECL_CONS_CDR(x);
					unlikely_if (!Null(x))
						goto ILLEGAL_LAMBDA;
                                        if (spp != ECL_NIL) assert_var_name(spp);
				}
			}
		}
		if (CONSP(v)) {
			key = ECL_CONS_CAR(v);
                        v = ECL_CONS_CDR(v);
			unlikely_if (ECL_ATOM(v) || !Null(ECL_CONS_CDR(v)))
				goto ILLEGAL_LAMBDA;
			v = ECL_CONS_CAR(v);
			if (context == @'function')
				assert_type_symbol(v);
			assert_type_symbol(key);
		} else {
			int intern_flag;
			key = ecl_intern(ecl_symbol_name(v), cl_core.keyword_package,
					 &intern_flag);
		}
        KEY_PUSH:
		nkey++;
		push(key, keys);
                assert_var_name(v);
		push(v, keys);
		push(init, keys);
                push(spp, keys);
		break;
        }
	default: {
                cl_object init;
		if (ECL_ATOM(v)) {
			init = ECL_NIL;
		} else if (Null(CDDR(v))) {
			cl_object x = v;
			v = ECL_CONS_CAR(x);
			init = CADR(x);
		} else
			goto ILLEGAL_LAMBDA;
		naux++;
                assert_var_name(v);
		push(v, auxs);
		push(init, auxs);
        }
	}
	goto LOOP;

OUTPUT:
	if ((nreq+nopt+(!Null(rest))+nkey) >= ECL_CALL_ARGUMENTS_LIMIT)
		FEprogram_error_noreturn("LAMBDA: Argument list ist too long, ~S.", 1,
				org_lambda_list);
	@(return CONS(ecl_make_fixnum(nreq), lists[0])
		 CONS(ecl_make_fixnum(nopt), lists[1])
		 rest
		 key_flag
		 CONS(ecl_make_fixnum(nkey), lists[2])
		 allow_other_keys
		 lists[3])

ILLEGAL_LAMBDA:
	FEprogram_error_noreturn("LAMBDA: Illegal lambda list ~S.", 1, org_lambda_list);

#undef push
#undef assert_var_name
}
