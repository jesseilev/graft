
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		return '<function>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$never = function (_p0) {
	never:
	while (true) {
		var _p1 = _p0;
		var _v1 = _p1._0;
		_p0 = _v1;
		continue never;
	}
};
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return A2(f, _p3._0, _p3._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$always = F2(
	function (a, _p4) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$JustOneMore = function (a) {
	return {ctor: 'JustOneMore', _0: a};
};

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		var _p1 = maybeValue;
		if (_p1.ctor === 'Just') {
			return callback(_p1._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p2 = maybe;
		if (_p2.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p3 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) && (_p3._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p3._0._0, _p3._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p4 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p4.ctor === '_Tuple3') && (_p4._0.ctor === 'Just')) && (_p4._1.ctor === 'Just')) && (_p4._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p4._0._0, _p4._1._0, _p4._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p5 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p5.ctor === '_Tuple4') && (_p5._0.ctor === 'Just')) && (_p5._1.ctor === 'Just')) && (_p5._2.ctor === 'Just')) && (_p5._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p5._0._0, _p5._1._0, _p5._2._0, _p5._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p6 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p6.ctor === '_Tuple5') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) && (_p6._2.ctor === 'Just')) && (_p6._3.ctor === 'Just')) && (_p6._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0, _p6._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$singleton = function (value) {
	return {
		ctor: '::',
		_0: value,
		_1: {ctor: '[]'}
	};
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			_elm_lang$core$List$any,
			function (_p2) {
				return !isOkay(_p2);
			},
			list);
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (front, back) {
				return pred(front) ? {ctor: '::', _0: front, _1: back} : back;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return {ctor: '::', _0: _p10._0, _1: xs};
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, x, _p11._0),
						_1: accAcc
					};
				} else {
					return {ctor: '[]'};
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				{
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				},
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: x, _1: _p16},
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: {ctor: '::', _0: x, _1: _p15}
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: _p19._0, _1: _p20._0},
				_1: {ctor: '::', _0: _p19._1, _1: _p20._1}
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var step = F2(
				function (x, rest) {
					return {
						ctor: '::',
						_0: sep,
						_1: {ctor: '::', _0: x, _1: rest}
					};
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				{ctor: '[]'},
				_p21._1);
			return {ctor: '::', _0: _p21._0, _1: spersed};
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = {ctor: '::', _0: _p22._0, _1: taken};
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				{ctor: '[]'}));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return {ctor: '[]'};
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return {
											ctor: '::',
											_0: _p23._1._0,
											_1: {
												ctor: '::',
												_0: _p23._1._1._0,
												_1: {ctor: '[]'}
											}
										};
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return {
												ctor: '::',
												_0: _p23._1._0,
												_1: {
													ctor: '::',
													_0: _p23._1._1._0,
													_1: {
														ctor: '::',
														_0: _p23._1._1._1._0,
														_1: {ctor: '[]'}
													}
												}
											};
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A2(_elm_lang$core$List$takeTailRec, n - 4, _p24)
														}
													}
												}
											} : {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)
														}
													}
												}
											};
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return {
					ctor: '::',
					_0: _p23._1._0,
					_1: {ctor: '[]'}
				};
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = {ctor: '::', _0: value, _1: result},
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			{ctor: '[]'},
			n,
			value);
	});
var _elm_lang$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(lo, hi) < 1) {
				var _v30 = lo,
					_v31 = hi - 1,
					_v32 = {ctor: '::', _0: hi, _1: list};
				lo = _v30;
				hi = _v31;
				list = _v32;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var _elm_lang$core$List$range = F2(
	function (lo, hi) {
		return A3(
			_elm_lang$core$List$rangeHelp,
			lo,
			hi,
			{ctor: '[]'});
	});
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (callback, result) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$mapError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _elm_lang$core$Native_List.fromArray(is);
}


function toInt(s)
{
	var len = s.length;

	// if empty
	if (len === 0)
	{
		return intErr(s);
	}

	// if hex
	var c = s[0];
	if (c === '0' && s[1] === 'x')
	{
		for (var i = 2; i < len; ++i)
		{
			var c = s[i];
			if (('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'))
			{
				continue;
			}
			return intErr(s);
		}
		return _elm_lang$core$Result$Ok(parseInt(s, 16));
	}

	// is decimal
	if (c > '9' || (c < '0' && c !== '-' && c !== '+'))
	{
		return intErr(s);
	}
	for (var i = 1; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return intErr(s);
		}
	}

	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function intErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int");
}


function toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return floatErr(s);
	}
	var n = +s;
	// faster isNaN check
	return n === n ? _elm_lang$core$Result$Ok(n) : floatErr(s);
}

function floatErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float");
}


function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Tuple$mapSecond = F2(
	function (func, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: func(_p1._1)
		};
	});
var _elm_lang$core$Tuple$mapFirst = F2(
	function (func, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: func(_p3._0),
			_1: _p3._1
		};
	});
var _elm_lang$core$Tuple$second = function (_p4) {
	var _p5 = _p4;
	return _p5._1;
};
var _elm_lang$core$Tuple$first = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};

//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function program(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flags !== 'undefined')
				{
					throw new Error(
						'The `' + moduleName + '` module does not need flags.\n'
						+ 'Call ' + moduleName + '.worker() with no arguments and you should be all set!'
					);
				}

				return initialize(
					impl.init,
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function programWithFlags(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flagDecoder === 'undefined')
				{
					throw new Error(
						'Are you trying to sneak a Never value into Elm? Trickster!\n'
						+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
						+ 'Use `program` instead if you do not want flags.'
					);
				}

				var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
				if (result.ctor === 'Err')
				{
					throw new Error(
						moduleName + '.worker(...) was called with an unexpected argument.\n'
						+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
						+ result._0
					);
				}

				return initialize(
					impl.init(result._0),
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function renderer(enqueue, _)
{
	return function(_) {};
}


// HTML TO PROGRAM

function htmlToProgram(vnode)
{
	var emptyBag = batch(_elm_lang$core$Native_List.Nil);
	var noChange = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		emptyBag
	);

	return _elm_lang$virtual_dom$VirtualDom$program({
		init: noChange,
		view: function(model) { return main; },
		update: F2(function(msg, model) { return noChange; }),
		subscriptions: function (model) { return emptyBag; }
	});
}


// INITIALIZE A PROGRAM

function initialize(init, update, subscriptions, renderer)
{
	// ambient state
	var managers = {};
	var updateView;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var model = init._0;
		updateView = renderer(enqueue, model);
		var cmds = init._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			updateView(model);
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, loop, handleMsg);
	}

	var task = A2(andThen, loop, init);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = converter(cmdList._0);
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(value)
	{
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		currentSend(result._0);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,

	htmlToProgram: htmlToProgram,
	program: program,
	programWithFlags: programWithFlags,
	initialize: initialize,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(callback, task)
{
	return {
		ctor: '_Task_andThen',
		callback: callback,
		task: task
	};
}

function onError(callback, task)
{
	return {
		ctor: '_Task_onError',
		callback: callback,
		task: task
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$programWithFlags = _elm_lang$core$Native_Platform.programWithFlags;
var _elm_lang$core$Platform$program = _elm_lang$core$Native_Platform.program;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		A2(
			_elm_lang$core$List$range,
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				{
					ctor: '::',
					_0: 'Internal red-black tree invariant violated, expected ',
					_1: {
						ctor: '::',
						_0: msg,
						_1: {
							ctor: '::',
							_0: ' and got ',
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(c),
								_1: {
									ctor: '::',
									_0: '/',
									_1: {
										ctor: '::',
										_0: lgot,
										_1: {
											ctor: '::',
											_0: '/',
											_1: {
												ctor: '::',
												_0: rgot,
												_1: {
													ctor: '::',
													_0: '\nPlease report this bug to <https://github.com/elm-lang/core/issues>',
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (color, left, right) {
		var _p29 = {ctor: '_Tuple2', _0: left, _1: right};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = color;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: color, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						color,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: color, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						color,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var newLeft = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, color, k, v, newLeft, right);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeIndex(index, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'index',
		index: index,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function mapMany(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function andThen(callback, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function map1(f, d1)
{
	return mapMany(f, [d1]);
}

function map2(f, d1, d2)
{
	return mapMany(f, [d1, d2]);
}

function map3(f, d1, d2, d3)
{
	return mapMany(f, [d1, d2, d3]);
}

function map4(f, d1, d2, d3, d4)
{
	return mapMany(f, [d1, d2, d3, d4]);
}

function map5(f, d1, d2, d3, d4, d5)
{
	return mapMany(f, [d1, d2, d3, d4, d5]);
}

function map6(f, d1, d2, d3, d4, d5, d6)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6]);
}

function map7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function map8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok') ? result : badField(field, result);

		case 'index':
			var index = decoder.index;
			if (!(value instanceof Array))
			{
				return badPrimitive('an array', value);
			}
			if (index >= value.length)
			{
				return badPrimitive('a longer array. Need index ' + index + ' but there are only ' + value.length + ' entries', value);
			}

			var result = runHelp(decoder.decoder, value[index]);
			return (result.tag === 'ok') ? result : badIndex(index, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'index':
			return a.index === b.index && equality(a.decoder, b.decoder);

		case 'map-many':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),
	decodeIndex: F2(decodeIndex),

	map1: F2(map1),
	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	map6: F7(map6),
	map7: F8(map7),
	map8: F9(map8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	andThen: F2(andThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$lazy = function (thunk) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		thunk,
		_elm_lang$core$Json_Decode$succeed(
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map8 = _elm_lang$core$Native_Json.map8;
var _elm_lang$core$Json_Decode$map7 = _elm_lang$core$Native_Json.map7;
var _elm_lang$core$Json_Decode$map6 = _elm_lang$core$Native_Json.map6;
var _elm_lang$core$Json_Decode$map5 = _elm_lang$core$Native_Json.map5;
var _elm_lang$core$Json_Decode$map4 = _elm_lang$core$Native_Json.map4;
var _elm_lang$core$Json_Decode$map3 = _elm_lang$core$Native_Json.map3;
var _elm_lang$core$Json_Decode$map2 = _elm_lang$core$Native_Json.map2;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.map1;
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$index = _elm_lang$core$Native_Json.decodeIndex;
var _elm_lang$core$Json_Decode$field = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(_elm_lang$core$List$foldr, _elm_lang$core$Json_Decode$field, decoder, fields);
	});
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$nullable = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

var _elm_lang$virtual_dom$VirtualDom_Debug$wrap;
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags;

var _elm_lang$virtual_dom$Native_VirtualDom = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';

var localDoc = typeof document !== 'undefined' ? document : {};


////////////  VIRTUAL DOM NODES  ////////////


function text(string)
{
	return {
		type: 'text',
		text: string
	};
}


function node(tag)
{
	return F2(function(factList, kidList) {
		return nodeHelp(tag, factList, kidList);
	});
}


function nodeHelp(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function keyedNode(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid._1.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'keyed-node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}


function map(tagger, node)
{
	return {
		type: 'tagger',
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
}


function thunk(func, args, thunk)
{
	return {
		type: 'thunk',
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

function lazy(fn, a)
{
	return thunk(fn, [a], function() {
		return fn(a);
	});
}

function lazy2(fn, a, b)
{
	return thunk(fn, [a,b], function() {
		return A2(fn, a, b);
	});
}

function lazy3(fn, a, b, c)
{
	return thunk(fn, [a,b,c], function() {
		return A3(fn, a, b, c);
	});
}



// FACTS


function organizeFacts(factList)
{
	var namespace, facts = {};

	while (factList.ctor !== '[]')
	{
		var entry = factList._0;
		var key = entry.key;

		if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
		{
			var subFacts = facts[key] || {};
			subFacts[entry.realKey] = entry.value;
			facts[key] = subFacts;
		}
		else if (key === STYLE_KEY)
		{
			var styles = facts[key] || {};
			var styleList = entry.value;
			while (styleList.ctor !== '[]')
			{
				var style = styleList._0;
				styles[style._0] = style._1;
				styleList = styleList._1;
			}
			facts[key] = styles;
		}
		else if (key === 'namespace')
		{
			namespace = entry.value;
		}
		else if (key === 'className')
		{
			var classes = facts[key];
			facts[key] = typeof classes === 'undefined'
				? entry.value
				: classes + ' ' + entry.value;
		}
 		else
		{
			facts[key] = entry.value;
		}
		factList = factList._1;
	}

	return {
		facts: facts,
		namespace: namespace
	};
}



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function style(value)
{
	return {
		key: STYLE_KEY,
		value: value
	};
}


function property(key, value)
{
	return {
		key: key,
		value: value
	};
}


function attribute(key, value)
{
	return {
		key: ATTR_KEY,
		realKey: key,
		value: value
	};
}


function attributeNS(namespace, key, value)
{
	return {
		key: ATTR_NS_KEY,
		realKey: key,
		value: {
			value: value,
			namespace: namespace
		}
	};
}


function on(name, options, decoder)
{
	return {
		key: EVENT_KEY,
		realKey: name,
		value: {
			options: options,
			decoder: decoder
		}
	};
}


function equalEvents(a, b)
{
	if (a.options !== b.options)
	{
		if (a.options.stopPropagation !== b.options.stopPropagation || a.options.preventDefault !== b.options.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}


function mapProperty(func, property)
{
	if (property.key !== EVENT_KEY)
	{
		return property;
	}
	return on(
		property.realKey,
		property.value.options,
		A2(_elm_lang$core$Json_Decode$map, func, property.value.decoder)
	);
}


////////////  RENDER  ////////////


function render(vNode, eventNode)
{
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);

		case 'tagger':
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.type === 'tagger')
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = { tagger: tagger, parent: eventNode };
			var domNode = render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case 'text':
			return localDoc.createTextNode(vNode.text);

		case 'node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i], eventNode));
			}

			return domNode;

		case 'keyed-node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i]._1, eventNode));
			}

			return domNode;

		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		switch (key)
		{
			case STYLE_KEY:
				applyStyles(domNode, value);
				break;

			case EVENT_KEY:
				applyEvents(domNode, eventNode, value);
				break;

			case ATTR_KEY:
				applyAttrs(domNode, value);
				break;

			case ATTR_NS_KEY:
				applyAttrsNS(domNode, value);
				break;

			case 'value':
				if (domNode[key] !== value)
				{
					domNode[key] = value;
				}
				break;

			default:
				domNode[key] = value;
				break;
		}
	}
}

function applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}

function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
			allHandlers[key] = undefined;
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}

function applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (typeof value === 'undefined')
		{
			domNode.removeAttribute(key);
		}
		else
		{
			domNode.setAttribute(key, value);
		}
	}
}

function applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.namespace;
		var value = pair.value;

		if (typeof value === 'undefined')
		{
			domNode.removeAttributeNS(namespace, key);
		}
		else
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}



////////////  DIFF  ////////////


function diff(a, b)
{
	var patches = [];
	diffHelp(a, b, patches, 0);
	return patches;
}


function makePatch(type, index, data)
{
	return {
		index: index,
		type: type,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
}


function diffHelp(a, b, patches, index)
{
	if (a === b)
	{
		return;
	}

	var aType = a.type;
	var bType = b.type;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (aType !== bType)
	{
		patches.push(makePatch('p-redraw', index, b));
		return;
	}

	// Now we know that both nodes are the same type.
	switch (bType)
	{
		case 'thunk':
			var aArgs = a.args;
			var bArgs = b.args;
			var i = aArgs.length;
			var same = a.func === b.func && i === bArgs.length;
			while (same && i--)
			{
				same = aArgs[i] === bArgs[i];
			}
			if (same)
			{
				b.node = a.node;
				return;
			}
			b.node = b.thunk();
			var subPatches = [];
			diffHelp(a.node, b.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(makePatch('p-thunk', index, subPatches));
			}
			return;

		case 'tagger':
			// gather nested taggers
			var aTaggers = a.tagger;
			var bTaggers = b.tagger;
			var nesting = false;

			var aSubNode = a.node;
			while (aSubNode.type === 'tagger')
			{
				nesting = true;

				typeof aTaggers !== 'object'
					? aTaggers = [aTaggers, aSubNode.tagger]
					: aTaggers.push(aSubNode.tagger);

				aSubNode = aSubNode.node;
			}

			var bSubNode = b.node;
			while (bSubNode.type === 'tagger')
			{
				nesting = true;

				typeof bTaggers !== 'object'
					? bTaggers = [bTaggers, bSubNode.tagger]
					: bTaggers.push(bSubNode.tagger);

				bSubNode = bSubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && aTaggers.length !== bTaggers.length)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
			{
				patches.push(makePatch('p-tagger', index, bTaggers));
			}

			// diff everything below the taggers
			diffHelp(aSubNode, bSubNode, patches, index + 1);
			return;

		case 'text':
			if (a.text !== b.text)
			{
				patches.push(makePatch('p-text', index, b.text));
				return;
			}

			return;

		case 'node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffChildren(a, b, patches, index);
			return;

		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;

		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
	}
}


// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function diffFacts(a, b, category)
{
	var diff;

	// look for changes and removals
	for (var aKey in a)
	{
		if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
		{
			var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[aKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(aKey in b))
		{
			diff = diff || {};
			diff[aKey] =
				(typeof category === 'undefined')
					? (typeof a[aKey] === 'string' ? '' : null)
					:
				(category === STYLE_KEY)
					? ''
					:
				(category === EVENT_KEY || category === ATTR_KEY)
					? undefined
					:
				{ namespace: a[aKey].namespace, value: undefined };

			continue;
		}

		var aValue = a[aKey];
		var bValue = b[aKey];

		// reference equal, so don't worry about it
		if (aValue === bValue && aKey !== 'value'
			|| category === EVENT_KEY && equalEvents(aValue, bValue))
		{
			continue;
		}

		diff = diff || {};
		diff[aKey] = bValue;
	}

	// add new stuff
	for (var bKey in b)
	{
		if (!(bKey in a))
		{
			diff = diff || {};
			diff[bKey] = b[bKey];
		}
	}

	return diff;
}


function diffChildren(aParent, bParent, patches, rootIndex)
{
	var aChildren = aParent.children;
	var bChildren = bParent.children;

	var aLen = aChildren.length;
	var bLen = bChildren.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (aLen > bLen)
	{
		patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
	}
	else if (aLen < bLen)
	{
		patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = aLen < bLen ? aLen : bLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var aChild = aChildren[i];
		diffHelp(aChild, bChildren[i], patches, index);
		index += aChild.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === 'p-reorder')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 'p-remove')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			var subNode = vNode.node;

			while (subNode.type === "tagger")
			{
				subNode = subNode.node;
			}

			return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'keyed-node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j]._1;
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function applyPatch(domNode, patch)
{
	switch (patch.type)
	{
		case 'p-redraw':
			return applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case 'p-facts':
			applyFacts(domNode, patch.eventNode, patch.data);
			return domNode;

		case 'p-text':
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case 'p-thunk':
			return applyPatchesHelp(domNode, patch.data);

		case 'p-tagger':
			if (typeof domNode.elm_event_node_ref !== 'undefined')
			{
				domNode.elm_event_node_ref.tagger = patch.data;
			}
			else
			{
				domNode.elm_event_node_ref = { tagger: patch.data, parent: patch.eventNode };
			}
			return domNode;

		case 'p-remove-last':
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case 'p-append':
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			return applyPatchReorder(domNode, patch);

		case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			throw new Error('Ran into an unknown patch!');
	}
}


function applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = localDoc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}


// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}


// DEBUG SETUP

function debugSetup(impl, object, moduleName, flagChecker)
{
	object['fullscreen'] = function fullscreen(flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, document.body, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};

	object['embed'] = function fullscreen(node, flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, node, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};
}

function scrollTask(popoutRef)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var doc = popoutRef.doc;
		if (doc)
		{
			var msgs = doc.getElementsByClassName('debugger-sidebar-messages')[0];
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


function debugRenderer(moduleName, parentNode, popoutRef, view, viewIn, viewOut)
{
	return function(tagger, initialModel)
	{
		var appEventNode = { tagger: tagger, parent: undefined };
		var eventNode = { tagger: tagger, parent: undefined };

		// make normal stepper
		var appVirtualNode = view(initialModel);
		var appNode = render(appVirtualNode, appEventNode);
		parentNode.appendChild(appNode);
		var appStepper = makeStepper(appNode, view, appVirtualNode, appEventNode);

		// make overlay stepper
		var overVirtualNode = viewIn(initialModel)._1;
		var overNode = render(overVirtualNode, eventNode);
		parentNode.appendChild(overNode);
		var wrappedViewIn = wrapViewIn(appEventNode, overNode, viewIn);
		var overStepper = makeStepper(overNode, wrappedViewIn, overVirtualNode, eventNode);

		// make debugger stepper
		var debugStepper = makeDebugStepper(initialModel, viewOut, eventNode, parentNode, moduleName, popoutRef);

		return function stepper(model)
		{
			appStepper(model);
			overStepper(model);
			debugStepper(model);
		}
	};
}

function makeDebugStepper(initialModel, view, eventNode, parentNode, moduleName, popoutRef)
{
	var curr;
	var domNode;

	return function stepper(model)
	{
		if (!model.isDebuggerOpen)
		{
			return;
		}

		if (!popoutRef.doc)
		{
			curr = view(model);
			domNode = openDebugWindow(moduleName, popoutRef, curr, eventNode);
			return;
		}

		// switch to document of popout
		localDoc = popoutRef.doc;

		var next = view(model);
		var patches = diff(curr, next);
		domNode = applyPatches(domNode, curr, patches, eventNode);
		curr = next;

		// switch back to normal document
		localDoc = document;
	};
}

function openDebugWindow(moduleName, popoutRef, virtualNode, eventNode)
{
	var w = 900;
	var h = 360;
	var x = screen.width - w;
	var y = screen.height - h;
	var debugWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);

	// switch to window document
	localDoc = debugWindow.document;

	popoutRef.doc = localDoc;
	localDoc.title = 'Debugger - ' + moduleName;
	localDoc.body.style.margin = '0';
	localDoc.body.style.padding = '0';
	var domNode = render(virtualNode, eventNode);
	localDoc.body.appendChild(domNode);

	localDoc.addEventListener('keydown', function(event) {
		if (event.metaKey && event.which === 82)
		{
			window.location.reload();
		}
		if (event.which === 38)
		{
			eventNode.tagger({ ctor: 'Up' });
			event.preventDefault();
		}
		if (event.which === 40)
		{
			eventNode.tagger({ ctor: 'Down' });
			event.preventDefault();
		}
	});

	function close()
	{
		popoutRef.doc = undefined;
		debugWindow.close();
	}
	window.addEventListener('unload', close);
	debugWindow.addEventListener('unload', function() {
		popoutRef.doc = undefined;
		window.removeEventListener('unload', close);
		eventNode.tagger({ ctor: 'Close' });
	});

	// switch back to the normal document
	localDoc = document;

	return domNode;
}


// BLOCK EVENTS

function wrapViewIn(appEventNode, overlayNode, viewIn)
{
	var ignorer = makeIgnorer(overlayNode);
	var blocking = 'Normal';
	var overflow;

	var normalTagger = appEventNode.tagger;
	var blockTagger = function() {};

	return function(model)
	{
		var tuple = viewIn(model);
		var newBlocking = tuple._0.ctor;
		appEventNode.tagger = newBlocking === 'Normal' ? normalTagger : blockTagger;
		if (blocking !== newBlocking)
		{
			traverse('removeEventListener', ignorer, blocking);
			traverse('addEventListener', ignorer, newBlocking);

			if (blocking === 'Normal')
			{
				overflow = document.body.style.overflow;
				document.body.style.overflow = 'hidden';
			}

			if (newBlocking === 'Normal')
			{
				document.body.style.overflow = overflow;
			}

			blocking = newBlocking;
		}
		return tuple._1;
	}
}

function traverse(verbEventListener, ignorer, blocking)
{
	switch(blocking)
	{
		case 'Normal':
			return;

		case 'Pause':
			return traverseHelp(verbEventListener, ignorer, mostEvents);

		case 'Message':
			return traverseHelp(verbEventListener, ignorer, allEvents);
	}
}

function traverseHelp(verbEventListener, handler, eventNames)
{
	for (var i = 0; i < eventNames.length; i++)
	{
		document.body[verbEventListener](eventNames[i], handler, true);
	}
}

function makeIgnorer(overlayNode)
{
	return function(event)
	{
		if (event.type === 'keydown' && event.metaKey && event.which === 82)
		{
			return;
		}

		var isScroll = event.type === 'scroll' || event.type === 'wheel';

		var node = event.target;
		while (node !== null)
		{
			if (node.className === 'elm-overlay-message-details' && isScroll)
			{
				return;
			}

			if (node === overlayNode && !isScroll)
			{
				return;
			}
			node = node.parentNode;
		}

		event.stopPropagation();
		event.preventDefault();
	}
}

var mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var allEvents = mostEvents.concat('wheel', 'scroll');


return {
	node: node,
	text: text,
	custom: custom,
	map: F2(map),

	on: F3(on),
	style: style,
	property: F2(property),
	attribute: F2(attribute),
	attributeNS: F3(attributeNS),
	mapProperty: F2(mapProperty),

	lazy: F2(lazy),
	lazy2: F3(lazy2),
	lazy3: F4(lazy3),
	keyedNode: F3(keyedNode),

	program: program,
	programWithFlags: programWithFlags,
	staticProgram: staticProgram
};

}();

var _elm_lang$virtual_dom$VirtualDom$programWithFlags = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.programWithFlags, _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags, impl);
};
var _elm_lang$virtual_dom$VirtualDom$program = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, impl);
};
var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom$mapProperty = _elm_lang$virtual_dom$Native_VirtualDom.mapProperty;
var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};

var _elm_lang$html$Html$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
var _elm_lang$html$Html$program = _elm_lang$virtual_dom$VirtualDom$program;
var _elm_lang$html$Html$beginnerProgram = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$html$Html$program(
		{
			init: A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_p1.model,
				{ctor: '[]'}),
			update: F2(
				function (msg, model) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						A2(_p1.update, msg, model),
						{ctor: '[]'});
				}),
			view: _p1.view,
			subscriptions: function (_p2) {
				return _elm_lang$core$Platform_Sub$none;
			}
		});
};
var _elm_lang$html$Html$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
var _elm_lang$html$Html$main_ = _elm_lang$html$Html$node('main');
var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');

var _elm_lang$html$Html_Attributes$map = _elm_lang$virtual_dom$VirtualDom$mapProperty;
var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
};
var _elm_lang$html$Html_Attributes$draggable = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
};
var _elm_lang$html$Html_Attributes$itemprop = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'itemprop', value);
};
var _elm_lang$html$Html_Attributes$tabindex = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'tabIndex',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$charset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'charset', value);
};
var _elm_lang$html$Html_Attributes$height = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'height',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$width = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'width',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$formaction = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'formAction', value);
};
var _elm_lang$html$Html_Attributes$list = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
};
var _elm_lang$html$Html_Attributes$minlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'minLength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$maxlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'maxlength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$size = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'size',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$form = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'form', value);
};
var _elm_lang$html$Html_Attributes$cols = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'cols',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rows = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rows',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$challenge = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'challenge', value);
};
var _elm_lang$html$Html_Attributes$media = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'media', value);
};
var _elm_lang$html$Html_Attributes$rel = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'rel', value);
};
var _elm_lang$html$Html_Attributes$datetime = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
};
var _elm_lang$html$Html_Attributes$pubdate = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
};
var _elm_lang$html$Html_Attributes$colspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'colspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rowspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rowspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$manifest = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'manifest', value);
};
var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
var _elm_lang$html$Html_Attributes$stringProperty = F2(
	function (name, string) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$string(string));
	});
var _elm_lang$html$Html_Attributes$class = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
};
var _elm_lang$html$Html_Attributes$id = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
};
var _elm_lang$html$Html_Attributes$title = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
};
var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'accessKey',
		_elm_lang$core$String$fromChar($char));
};
var _elm_lang$html$Html_Attributes$dir = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
};
var _elm_lang$html$Html_Attributes$dropzone = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
};
var _elm_lang$html$Html_Attributes$lang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
};
var _elm_lang$html$Html_Attributes$content = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
};
var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
};
var _elm_lang$html$Html_Attributes$language = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
};
var _elm_lang$html$Html_Attributes$src = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
};
var _elm_lang$html$Html_Attributes$alt = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
};
var _elm_lang$html$Html_Attributes$preload = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
};
var _elm_lang$html$Html_Attributes$poster = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
};
var _elm_lang$html$Html_Attributes$kind = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
};
var _elm_lang$html$Html_Attributes$srclang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
};
var _elm_lang$html$Html_Attributes$sandbox = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
};
var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
};
var _elm_lang$html$Html_Attributes$type_ = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
};
var _elm_lang$html$Html_Attributes$value = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
};
var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
};
var _elm_lang$html$Html_Attributes$placeholder = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
};
var _elm_lang$html$Html_Attributes$accept = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
};
var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
};
var _elm_lang$html$Html_Attributes$action = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
};
var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var _elm_lang$html$Html_Attributes$enctype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
};
var _elm_lang$html$Html_Attributes$method = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
};
var _elm_lang$html$Html_Attributes$name = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
};
var _elm_lang$html$Html_Attributes$pattern = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
};
var _elm_lang$html$Html_Attributes$for = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
};
var _elm_lang$html$Html_Attributes$max = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
};
var _elm_lang$html$Html_Attributes$min = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
};
var _elm_lang$html$Html_Attributes$step = function (n) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
};
var _elm_lang$html$Html_Attributes$wrap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
};
var _elm_lang$html$Html_Attributes$usemap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
};
var _elm_lang$html$Html_Attributes$shape = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
};
var _elm_lang$html$Html_Attributes$coords = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
};
var _elm_lang$html$Html_Attributes$keytype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
};
var _elm_lang$html$Html_Attributes$align = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
};
var _elm_lang$html$Html_Attributes$cite = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
};
var _elm_lang$html$Html_Attributes$href = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
};
var _elm_lang$html$Html_Attributes$target = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
};
var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
};
var _elm_lang$html$Html_Attributes$hreflang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
};
var _elm_lang$html$Html_Attributes$ping = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
};
var _elm_lang$html$Html_Attributes$start = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'start',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$headers = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
};
var _elm_lang$html$Html_Attributes$scope = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
};
var _elm_lang$html$Html_Attributes$boolProperty = F2(
	function (name, bool) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$bool(bool));
	});
var _elm_lang$html$Html_Attributes$hidden = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
};
var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
};
var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
};
var _elm_lang$html$Html_Attributes$async = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
};
var _elm_lang$html$Html_Attributes$defer = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
};
var _elm_lang$html$Html_Attributes$scoped = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
};
var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
};
var _elm_lang$html$Html_Attributes$controls = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
};
var _elm_lang$html$Html_Attributes$loop = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
};
var _elm_lang$html$Html_Attributes$default = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
};
var _elm_lang$html$Html_Attributes$seamless = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
};
var _elm_lang$html$Html_Attributes$checked = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
};
var _elm_lang$html$Html_Attributes$selected = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
};
var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
};
var _elm_lang$html$Html_Attributes$disabled = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
};
var _elm_lang$html$Html_Attributes$multiple = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
};
var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
};
var _elm_lang$html$Html_Attributes$readonly = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
};
var _elm_lang$html$Html_Attributes$required = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
};
var _elm_lang$html$Html_Attributes$ismap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
};
var _elm_lang$html$Html_Attributes$download = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
};
var _elm_lang$html$Html_Attributes$reversed = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
};
var _elm_lang$html$Html_Attributes$classList = function (list) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(_elm_lang$core$List$filter, _elm_lang$core$Tuple$second, list))));
};
var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;

var _arturopala$elm_monocle$Monocle_Iso$modify = F2(
	function (iso, f) {
		return function (_p0) {
			return iso.reverseGet(
				f(
					iso.get(_p0)));
		};
	});
var _arturopala$elm_monocle$Monocle_Iso$Iso = F2(
	function (a, b) {
		return {get: a, reverseGet: b};
	});
var _arturopala$elm_monocle$Monocle_Iso$reverse = function (iso) {
	return A2(_arturopala$elm_monocle$Monocle_Iso$Iso, iso.reverseGet, iso.get);
};
var _arturopala$elm_monocle$Monocle_Iso$compose = F2(
	function (outer, inner) {
		return A2(
			_arturopala$elm_monocle$Monocle_Iso$Iso,
			function (_p1) {
				return inner.get(
					outer.get(_p1));
			},
			function (_p2) {
				return outer.reverseGet(
					inner.reverseGet(_p2));
			});
	});

var _arturopala$elm_monocle$Monocle_Lens$modifyAndMerge = F3(
	function (lens, fx, merge) {
		var mf = function (_p0) {
			var _p1 = _p0;
			var _p4 = _p1._0;
			return function (_p2) {
				var _p3 = _p2;
				return {
					ctor: '_Tuple2',
					_0: A2(lens.set, _p3._0, _p4),
					_1: A2(merge, _p1._1, _p3._1)
				};
			}(
				fx(
					lens.get(_p4)));
		};
		return mf;
	});
var _arturopala$elm_monocle$Monocle_Lens$modify3 = F4(
	function (lens1, lens2, lens3, fx) {
		var mf = function (_p5) {
			var _p6 = _p5;
			var _p11 = _p6._2;
			var _p10 = _p6._1;
			var _p9 = _p6._0;
			return function (_p7) {
				var _p8 = _p7;
				return {
					ctor: '_Tuple3',
					_0: A2(lens1.set, _p8._0, _p9),
					_1: A2(lens2.set, _p8._1, _p10),
					_2: A2(lens3.set, _p8._2, _p11)
				};
			}(
				fx(
					{
						ctor: '_Tuple3',
						_0: lens1.get(_p9),
						_1: lens2.get(_p10),
						_2: lens3.get(_p11)
					}));
		};
		return mf;
	});
var _arturopala$elm_monocle$Monocle_Lens$modify2 = F3(
	function (lens1, lens2, fx) {
		var mf = function (_p12) {
			var _p13 = _p12;
			var _p17 = _p13._1;
			var _p16 = _p13._0;
			return function (_p14) {
				var _p15 = _p14;
				return {
					ctor: '_Tuple2',
					_0: A2(lens1.set, _p15._0, _p16),
					_1: A2(lens2.set, _p15._1, _p17)
				};
			}(
				fx(
					{
						ctor: '_Tuple2',
						_0: lens1.get(_p16),
						_1: lens2.get(_p17)
					}));
		};
		return mf;
	});
var _arturopala$elm_monocle$Monocle_Lens$modify = F2(
	function (lens, f) {
		var mf = function (a) {
			return function (b) {
				return A2(lens.set, b, a);
			}(
				f(
					lens.get(a)));
		};
		return mf;
	});
var _arturopala$elm_monocle$Monocle_Lens$Lens = F2(
	function (a, b) {
		return {get: a, set: b};
	});
var _arturopala$elm_monocle$Monocle_Lens$compose = F2(
	function (outer, inner) {
		var set = F2(
			function (c, a) {
				return function (b) {
					return A2(outer.set, b, a);
				}(
					A2(
						inner.set,
						c,
						outer.get(a)));
			});
		return A2(
			_arturopala$elm_monocle$Monocle_Lens$Lens,
			function (_p18) {
				return inner.get(
					outer.get(_p18));
			},
			set);
	});
var _arturopala$elm_monocle$Monocle_Lens$fromIso = function (iso) {
	var set = F2(
		function (b, _p19) {
			return iso.reverseGet(b);
		});
	return A2(_arturopala$elm_monocle$Monocle_Lens$Lens, iso.get, set);
};
var _arturopala$elm_monocle$Monocle_Lens$zip = F2(
	function (left, right) {
		var set = F2(
			function (_p21, _p20) {
				var _p22 = _p21;
				var _p23 = _p20;
				return {
					ctor: '_Tuple2',
					_0: A2(left.set, _p22._0, _p23._0),
					_1: A2(right.set, _p22._1, _p23._1)
				};
			});
		var get = function (_p24) {
			var _p25 = _p24;
			return {
				ctor: '_Tuple2',
				_0: left.get(_p25._0),
				_1: right.get(_p25._1)
			};
		};
		return A2(_arturopala$elm_monocle$Monocle_Lens$Lens, get, set);
	});
var _arturopala$elm_monocle$Monocle_Lens$tuple = F2(
	function (left, right) {
		var set = F2(
			function (_p26, a) {
				var _p27 = _p26;
				return A2(
					right.set,
					_p27._1,
					A2(left.set, _p27._0, a));
			});
		var get = function (a) {
			return {
				ctor: '_Tuple2',
				_0: left.get(a),
				_1: right.get(a)
			};
		};
		return A2(_arturopala$elm_monocle$Monocle_Lens$Lens, get, set);
	});
var _arturopala$elm_monocle$Monocle_Lens$tuple3 = F3(
	function (first, second, third) {
		var set = F2(
			function (_p28, a) {
				var _p29 = _p28;
				return A2(
					third.set,
					_p29._2,
					A2(
						second.set,
						_p29._1,
						A2(first.set, _p29._0, a)));
			});
		var get = function (a) {
			return {
				ctor: '_Tuple3',
				_0: first.get(a),
				_1: second.get(a),
				_2: third.get(a)
			};
		};
		return A2(_arturopala$elm_monocle$Monocle_Lens$Lens, get, set);
	});

var _avh4$elm_fifo$Fifo$toList = function (_p0) {
	var _p1 = _p0;
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_p1._0,
		_elm_lang$core$List$reverse(_p1._1));
};
var _avh4$elm_fifo$Fifo$Fifo = F2(
	function (a, b) {
		return {ctor: 'Fifo', _0: a, _1: b};
	});
var _avh4$elm_fifo$Fifo$empty = A2(
	_avh4$elm_fifo$Fifo$Fifo,
	{ctor: '[]'},
	{ctor: '[]'});
var _avh4$elm_fifo$Fifo$insert = F2(
	function (a, _p2) {
		var _p3 = _p2;
		return A2(
			_avh4$elm_fifo$Fifo$Fifo,
			_p3._0,
			{ctor: '::', _0: a, _1: _p3._1});
	});
var _avh4$elm_fifo$Fifo$remove = function (fifo) {
	remove:
	while (true) {
		var _p4 = fifo;
		if (_p4._0.ctor === '[]') {
			if (_p4._1.ctor === '[]') {
				return {ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: _avh4$elm_fifo$Fifo$empty};
			} else {
				var _v3 = A2(
					_avh4$elm_fifo$Fifo$Fifo,
					_elm_lang$core$List$reverse(_p4._1),
					{ctor: '[]'});
				fifo = _v3;
				continue remove;
			}
		} else {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Maybe$Just(_p4._0._0),
				_1: A2(_avh4$elm_fifo$Fifo$Fifo, _p4._0._1, _p4._1)
			};
		}
	}
};
var _avh4$elm_fifo$Fifo$fromList = function (list) {
	return A2(
		_avh4$elm_fifo$Fifo$Fifo,
		list,
		{ctor: '[]'});
};

var _elm_community$graph$Graph_Tree$pushMany = F2(
	function (vals, queue) {
		return A3(_elm_lang$core$List$foldl, _avh4$elm_fifo$Fifo$insert, queue, vals);
	});
var _elm_community$graph$Graph_Tree$listForTraversal = F2(
	function (traversal, tree) {
		var acc = _elm_lang$core$Basics$identity;
		var f = F3(
			function (label, children, rest) {
				return function (_p0) {
					return rest(
						A2(
							F2(
								function (x, y) {
									return {ctor: '::', _0: x, _1: y};
								}),
							label,
							_p0));
				};
			});
		return A4(
			traversal,
			f,
			acc,
			tree,
			{ctor: '[]'});
	});
var _elm_community$graph$Graph_Tree$size = function (tree) {
	var _p1 = tree;
	return _p1._0;
};
var _elm_community$graph$Graph_Tree$root = function (tree) {
	var _p2 = tree;
	return _p2._1;
};
var _elm_community$graph$Graph_Tree$height = function (tree) {
	var go = F2(
		function (h, t) {
			var _p3 = _elm_community$graph$Graph_Tree$root(t);
			if (_p3.ctor === 'Just') {
				return A3(
					_elm_lang$core$List$foldl,
					function (_p4) {
						return _elm_lang$core$Basics$max(
							A2(go, h + 1, _p4));
					},
					h + 1,
					_p3._0._1);
			} else {
				return h;
			}
		});
	return A2(go, 0, tree);
};
var _elm_community$graph$Graph_Tree$levelOrder = F3(
	function (visit, acc, tree) {
		var go = F2(
			function (acc, toVisit) {
				go:
				while (true) {
					var _p5 = _avh4$elm_fifo$Fifo$remove(toVisit);
					if (_p5._0.ctor === 'Nothing') {
						return acc;
					} else {
						var _p8 = _p5._1;
						var _p6 = _elm_community$graph$Graph_Tree$root(_p5._0._0);
						if (_p6.ctor === 'Nothing') {
							var _v5 = acc,
								_v6 = _p8;
							acc = _v5;
							toVisit = _v6;
							continue go;
						} else {
							var _p7 = _p6._0._1;
							var _v7 = A3(visit, _p6._0._0, _p7, acc),
								_v8 = A2(_elm_community$graph$Graph_Tree$pushMany, _p7, _p8);
							acc = _v7;
							toVisit = _v8;
							continue go;
						}
					}
				}
			});
		return A2(
			go,
			acc,
			A2(_avh4$elm_fifo$Fifo$insert, tree, _avh4$elm_fifo$Fifo$empty));
	});
var _elm_community$graph$Graph_Tree$levelOrderList = _elm_community$graph$Graph_Tree$listForTraversal(_elm_community$graph$Graph_Tree$levelOrder);
var _elm_community$graph$Graph_Tree$postOrder = F3(
	function (visit, acc, tree) {
		var folder = _elm_lang$core$Basics$flip(
			_elm_community$graph$Graph_Tree$postOrder(visit));
		var _p9 = _elm_community$graph$Graph_Tree$root(tree);
		if (_p9.ctor === 'Nothing') {
			return acc;
		} else {
			var _p10 = _p9._0._1;
			return A3(
				visit,
				_p9._0._0,
				_p10,
				A3(_elm_lang$core$List$foldl, folder, acc, _p10));
		}
	});
var _elm_community$graph$Graph_Tree$postOrderList = _elm_community$graph$Graph_Tree$listForTraversal(_elm_community$graph$Graph_Tree$postOrder);
var _elm_community$graph$Graph_Tree$preOrder = F3(
	function (visit, acc, tree) {
		var folder = _elm_lang$core$Basics$flip(
			_elm_community$graph$Graph_Tree$preOrder(visit));
		var _p11 = _elm_community$graph$Graph_Tree$root(tree);
		if (_p11.ctor === 'Nothing') {
			return acc;
		} else {
			var _p12 = _p11._0._1;
			return A3(
				_elm_lang$core$List$foldl,
				folder,
				A3(visit, _p11._0._0, _p12, acc),
				_p12);
		}
	});
var _elm_community$graph$Graph_Tree$preOrderList = _elm_community$graph$Graph_Tree$listForTraversal(_elm_community$graph$Graph_Tree$preOrder);
var _elm_community$graph$Graph_Tree$MkTree = F2(
	function (a, b) {
		return {ctor: 'MkTree', _0: a, _1: b};
	});
var _elm_community$graph$Graph_Tree$empty = A2(_elm_community$graph$Graph_Tree$MkTree, 0, _elm_lang$core$Maybe$Nothing);
var _elm_community$graph$Graph_Tree$isEmpty = function (tree) {
	return _elm_lang$core$Native_Utils.eq(tree, _elm_community$graph$Graph_Tree$empty);
};
var _elm_community$graph$Graph_Tree$inner = F2(
	function (label, children) {
		var nonEmptyChildren = A2(
			_elm_lang$core$List$filter,
			function (_p13) {
				return !_elm_community$graph$Graph_Tree$isEmpty(_p13);
			},
			children);
		var totalSize = A3(
			_elm_lang$core$List$foldl,
			function (_p14) {
				return F2(
					function (x, y) {
						return x + y;
					})(
					_elm_community$graph$Graph_Tree$size(_p14));
			},
			1,
			nonEmptyChildren);
		return A2(
			_elm_community$graph$Graph_Tree$MkTree,
			totalSize,
			_elm_lang$core$Maybe$Just(
				{ctor: '_Tuple2', _0: label, _1: nonEmptyChildren}));
	});
var _elm_community$graph$Graph_Tree$leaf = function (val) {
	return A2(
		_elm_community$graph$Graph_Tree$inner,
		val,
		{ctor: '[]'});
};
var _elm_community$graph$Graph_Tree$unfoldTree = F2(
	function (next, seed) {
		var _p15 = next(seed);
		var label = _p15._0;
		var seeds = _p15._1;
		return A2(
			_elm_community$graph$Graph_Tree$inner,
			label,
			A2(
				_elm_lang$core$List$map,
				_elm_community$graph$Graph_Tree$unfoldTree(next),
				seeds));
	});
var _elm_community$graph$Graph_Tree$unfoldForest = F2(
	function (next, seeds) {
		return A2(
			_elm_lang$core$List$map,
			_elm_community$graph$Graph_Tree$unfoldTree(next),
			seeds);
	});

var _elm_lang$core$Native_Bitwise = function() {

return {
	and: F2(function and(a, b) { return a & b; }),
	or: F2(function or(a, b) { return a | b; }),
	xor: F2(function xor(a, b) { return a ^ b; }),
	complement: function complement(a) { return ~a; },
	shiftLeftBy: F2(function(offset, a) { return a << offset; }),
	shiftRightBy: F2(function(offset, a) { return a >> offset; }),
	shiftRightZfBy: F2(function(offset, a) { return a >>> offset; })
};

}();

var _elm_lang$core$Bitwise$shiftRightZfBy = _elm_lang$core$Native_Bitwise.shiftRightZfBy;
var _elm_lang$core$Bitwise$shiftRightBy = _elm_lang$core$Native_Bitwise.shiftRightBy;
var _elm_lang$core$Bitwise$shiftLeftBy = _elm_lang$core$Native_Bitwise.shiftLeftBy;
var _elm_lang$core$Bitwise$complement = _elm_lang$core$Native_Bitwise.complement;
var _elm_lang$core$Bitwise$xor = _elm_lang$core$Native_Bitwise.xor;
var _elm_lang$core$Bitwise$or = _elm_lang$core$Native_Bitwise.or;
var _elm_lang$core$Bitwise$and = _elm_lang$core$Native_Bitwise.and;

var _elm_community$intdict$IntDict$combineBits = F3(
	function (a, b, mask) {
		return (a & (~mask)) | (b & mask);
	});
var _elm_community$intdict$IntDict$foldr = F3(
	function (f, acc, dict) {
		foldr:
		while (true) {
			var _p0 = dict;
			switch (_p0.ctor) {
				case 'Empty':
					return acc;
				case 'Leaf':
					var _p1 = _p0._0;
					return A3(f, _p1.key, _p1.value, acc);
				default:
					var _p2 = _p0._0;
					var _v1 = f,
						_v2 = A3(_elm_community$intdict$IntDict$foldr, f, acc, _p2.right),
						_v3 = _p2.left;
					f = _v1;
					acc = _v2;
					dict = _v3;
					continue foldr;
			}
		}
	});
var _elm_community$intdict$IntDict$keys = function (dict) {
	return A3(
		_elm_community$intdict$IntDict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_community$intdict$IntDict$values = function (dict) {
	return A3(
		_elm_community$intdict$IntDict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_community$intdict$IntDict$toList = function (dict) {
	return A3(
		_elm_community$intdict$IntDict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_community$intdict$IntDict$toString = function (dict) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		'IntDict.fromList ',
		_elm_lang$core$Basics$toString(
			_elm_community$intdict$IntDict$toList(dict)));
};
var _elm_community$intdict$IntDict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p3 = dict;
			switch (_p3.ctor) {
				case 'Empty':
					return acc;
				case 'Leaf':
					var _p4 = _p3._0;
					return A3(f, _p4.key, _p4.value, acc);
				default:
					var _p5 = _p3._0;
					var _v5 = f,
						_v6 = A3(_elm_community$intdict$IntDict$foldl, f, acc, _p5.left),
						_v7 = _p5.right;
					f = _v5;
					acc = _v6;
					dict = _v7;
					continue foldl;
			}
		}
	});
var _elm_community$intdict$IntDict$findMax = function (dict) {
	findMax:
	while (true) {
		var _p6 = dict;
		switch (_p6.ctor) {
			case 'Empty':
				return _elm_lang$core$Maybe$Nothing;
			case 'Leaf':
				var _p7 = _p6._0;
				return _elm_lang$core$Maybe$Just(
					{ctor: '_Tuple2', _0: _p7.key, _1: _p7.value});
			default:
				var _v9 = _p6._0.right;
				dict = _v9;
				continue findMax;
		}
	}
};
var _elm_community$intdict$IntDict$findMin = function (dict) {
	findMin:
	while (true) {
		var _p8 = dict;
		switch (_p8.ctor) {
			case 'Empty':
				return _elm_lang$core$Maybe$Nothing;
			case 'Leaf':
				var _p9 = _p8._0;
				return _elm_lang$core$Maybe$Just(
					{ctor: '_Tuple2', _0: _p9.key, _1: _p9.value});
			default:
				var _v11 = _p8._0.left;
				dict = _v11;
				continue findMin;
		}
	}
};
var _elm_community$intdict$IntDict$size = function (dict) {
	var _p10 = dict;
	switch (_p10.ctor) {
		case 'Empty':
			return 0;
		case 'Leaf':
			return 1;
		default:
			return _p10._0.size;
	}
};
var _elm_community$intdict$IntDict$isEmpty = function (dict) {
	var _p11 = dict;
	if (_p11.ctor === 'Empty') {
		return true;
	} else {
		return false;
	}
};
var _elm_community$intdict$IntDict$highestBitSet = function (n) {
	var shiftOr = F2(
		function (i, shift) {
			return i | (i >>> shift);
		});
	var n1 = A2(shiftOr, n, 1);
	var n2 = A2(shiftOr, n1, 2);
	var n3 = A2(shiftOr, n2, 4);
	var n4 = A2(shiftOr, n3, 8);
	var n5 = A2(shiftOr, n4, 16);
	return n5 & (~(n5 >>> 1));
};
var _elm_community$intdict$IntDict$signBit = _elm_community$intdict$IntDict$highestBitSet(-1);
var _elm_community$intdict$IntDict$isBranchingBitSet = function (p) {
	return function (_p12) {
		return A2(
			F2(
				function (x, y) {
					return !_elm_lang$core$Native_Utils.eq(x, y);
				}),
			0,
			p.branchingBit & (_elm_community$intdict$IntDict$signBit ^ _p12));
	};
};
var _elm_community$intdict$IntDict$higherBitMask = function (branchingBit) {
	return ~((branchingBit * 2) - 1);
};
var _elm_community$intdict$IntDict$prefixMatches = F2(
	function (p, n) {
		return _elm_lang$core$Native_Utils.eq(
			n & _elm_community$intdict$IntDict$higherBitMask(p.branchingBit),
			p.prefixBits);
	});
var _elm_community$intdict$IntDict$get = F2(
	function (key, dict) {
		get:
		while (true) {
			var _p13 = dict;
			switch (_p13.ctor) {
				case 'Empty':
					return _elm_lang$core$Maybe$Nothing;
				case 'Leaf':
					var _p14 = _p13._0;
					return _elm_lang$core$Native_Utils.eq(_p14.key, key) ? _elm_lang$core$Maybe$Just(_p14.value) : _elm_lang$core$Maybe$Nothing;
				default:
					var _p15 = _p13._0;
					if (!A2(_elm_community$intdict$IntDict$prefixMatches, _p15.prefix, key)) {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						if (A2(_elm_community$intdict$IntDict$isBranchingBitSet, _p15.prefix, key)) {
							var _v15 = key,
								_v16 = _p15.right;
							key = _v15;
							dict = _v16;
							continue get;
						} else {
							var _v17 = key,
								_v18 = _p15.left;
							key = _v17;
							dict = _v18;
							continue get;
						}
					}
			}
		}
	});
var _elm_community$intdict$IntDict$member = F2(
	function (key, dict) {
		var _p16 = A2(_elm_community$intdict$IntDict$get, key, dict);
		if (_p16.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_community$intdict$IntDict$lcp = F2(
	function (x, y) {
		var diff = x ^ y;
		var branchingBit = _elm_community$intdict$IntDict$highestBitSet(diff);
		var mask = _elm_community$intdict$IntDict$higherBitMask(branchingBit);
		var prefixBits = x & mask;
		return {prefixBits: prefixBits, branchingBit: branchingBit};
	});
var _elm_community$intdict$IntDict$isValidKey = function (k) {
	return _elm_lang$core$Native_Utils.eq(k | 0, k);
};
var _elm_community$intdict$IntDict$KeyPrefix = F2(
	function (a, b) {
		return {prefixBits: a, branchingBit: b};
	});
var _elm_community$intdict$IntDict$InnerType = F4(
	function (a, b, c, d) {
		return {prefix: a, left: b, right: c, size: d};
	});
var _elm_community$intdict$IntDict$Inner = function (a) {
	return {ctor: 'Inner', _0: a};
};
var _elm_community$intdict$IntDict$inner = F3(
	function (p, l, r) {
		var _p17 = {ctor: '_Tuple2', _0: l, _1: r};
		if (_p17._0.ctor === 'Empty') {
			return r;
		} else {
			if (_p17._1.ctor === 'Empty') {
				return l;
			} else {
				return _elm_community$intdict$IntDict$Inner(
					{
						prefix: p,
						left: l,
						right: r,
						size: _elm_community$intdict$IntDict$size(l) + _elm_community$intdict$IntDict$size(r)
					});
			}
		}
	});
var _elm_community$intdict$IntDict$Leaf = function (a) {
	return {ctor: 'Leaf', _0: a};
};
var _elm_community$intdict$IntDict$leaf = F2(
	function (k, v) {
		return _elm_community$intdict$IntDict$Leaf(
			{key: k, value: v});
	});
var _elm_community$intdict$IntDict$singleton = F2(
	function (key, value) {
		return A2(_elm_community$intdict$IntDict$leaf, key, value);
	});
var _elm_community$intdict$IntDict$Empty = {ctor: 'Empty'};
var _elm_community$intdict$IntDict$empty = _elm_community$intdict$IntDict$Empty;
var _elm_community$intdict$IntDict$update = F3(
	function (key, alter, dict) {
		var join = F2(
			function (_p19, _p18) {
				var _p20 = _p19;
				var _p24 = _p20._1;
				var _p21 = _p18;
				var _p23 = _p21._1;
				var _p22 = _p21._0;
				var prefix = A2(_elm_community$intdict$IntDict$lcp, _p20._0, _p22);
				return A2(_elm_community$intdict$IntDict$isBranchingBitSet, prefix, _p22) ? A3(_elm_community$intdict$IntDict$inner, prefix, _p24, _p23) : A3(_elm_community$intdict$IntDict$inner, prefix, _p23, _p24);
			});
		var alteredNode = function (mv) {
			var _p25 = alter(mv);
			if (_p25.ctor === 'Just') {
				return A2(_elm_community$intdict$IntDict$leaf, key, _p25._0);
			} else {
				return _elm_community$intdict$IntDict$empty;
			}
		};
		var _p26 = dict;
		switch (_p26.ctor) {
			case 'Empty':
				return alteredNode(_elm_lang$core$Maybe$Nothing);
			case 'Leaf':
				var _p27 = _p26._0;
				return _elm_lang$core$Native_Utils.eq(_p27.key, key) ? alteredNode(
					_elm_lang$core$Maybe$Just(_p27.value)) : A2(
					join,
					{
						ctor: '_Tuple2',
						_0: key,
						_1: alteredNode(_elm_lang$core$Maybe$Nothing)
					},
					{ctor: '_Tuple2', _0: _p27.key, _1: dict});
			default:
				var _p28 = _p26._0;
				return A2(_elm_community$intdict$IntDict$prefixMatches, _p28.prefix, key) ? (A2(_elm_community$intdict$IntDict$isBranchingBitSet, _p28.prefix, key) ? A3(
					_elm_community$intdict$IntDict$inner,
					_p28.prefix,
					_p28.left,
					A3(_elm_community$intdict$IntDict$update, key, alter, _p28.right)) : A3(
					_elm_community$intdict$IntDict$inner,
					_p28.prefix,
					A3(_elm_community$intdict$IntDict$update, key, alter, _p28.left),
					_p28.right)) : A2(
					join,
					{
						ctor: '_Tuple2',
						_0: key,
						_1: alteredNode(_elm_lang$core$Maybe$Nothing)
					},
					{ctor: '_Tuple2', _0: _p28.prefix.prefixBits, _1: dict});
		}
	});
var _elm_community$intdict$IntDict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_community$intdict$IntDict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_community$intdict$IntDict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_community$intdict$IntDict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_community$intdict$IntDict$filter = F2(
	function (predicate, dict) {
		var add = F3(
			function (k, v, d) {
				return A2(predicate, k, v) ? A3(_elm_community$intdict$IntDict$insert, k, v, d) : d;
			});
		return A3(_elm_community$intdict$IntDict$foldl, add, _elm_community$intdict$IntDict$empty, dict);
	});
var _elm_community$intdict$IntDict$map = F2(
	function (f, dict) {
		var _p29 = dict;
		switch (_p29.ctor) {
			case 'Empty':
				return _elm_community$intdict$IntDict$empty;
			case 'Leaf':
				var _p30 = _p29._0;
				return A2(
					_elm_community$intdict$IntDict$leaf,
					_p30.key,
					A2(f, _p30.key, _p30.value));
			default:
				var _p31 = _p29._0;
				return A3(
					_elm_community$intdict$IntDict$inner,
					_p31.prefix,
					A2(_elm_community$intdict$IntDict$map, f, _p31.left),
					A2(_elm_community$intdict$IntDict$map, f, _p31.right));
		}
	});
var _elm_community$intdict$IntDict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p32) {
				var _p33 = _p32;
				var _p35 = _p33._1;
				var _p34 = _p33._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_community$intdict$IntDict$insert, key, value, _p34),
					_1: _p35
				} : {
					ctor: '_Tuple2',
					_0: _p34,
					_1: A3(_elm_community$intdict$IntDict$insert, key, value, _p35)
				};
			});
		return A3(
			_elm_community$intdict$IntDict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_community$intdict$IntDict$empty, _1: _elm_community$intdict$IntDict$empty},
			dict);
	});
var _elm_community$intdict$IntDict$fromList = function (pairs) {
	return A3(
		_elm_lang$core$List$foldl,
		_elm_lang$core$Basics$uncurry(_elm_community$intdict$IntDict$insert),
		_elm_community$intdict$IntDict$empty,
		pairs);
};
var _elm_community$intdict$IntDict$Right = {ctor: 'Right'};
var _elm_community$intdict$IntDict$Left = {ctor: 'Left'};
var _elm_community$intdict$IntDict$Disjunct = F2(
	function (a, b) {
		return {ctor: 'Disjunct', _0: a, _1: b};
	});
var _elm_community$intdict$IntDict$Parent = F2(
	function (a, b) {
		return {ctor: 'Parent', _0: a, _1: b};
	});
var _elm_community$intdict$IntDict$SamePrefix = {ctor: 'SamePrefix'};
var _elm_community$intdict$IntDict$determineBranchRelation = F2(
	function (l, r) {
		var childEdge = F2(
			function (prefix, c) {
				return A2(_elm_community$intdict$IntDict$isBranchingBitSet, prefix, c.prefix.prefixBits) ? _elm_community$intdict$IntDict$Right : _elm_community$intdict$IntDict$Left;
			});
		var rp = r.prefix;
		var lp = l.prefix;
		var mask = _elm_community$intdict$IntDict$highestBitSet(
			A2(_elm_lang$core$Basics$max, lp.branchingBit, rp.branchingBit));
		var modifiedRightPrefix = A3(_elm_community$intdict$IntDict$combineBits, rp.prefixBits, ~lp.prefixBits, mask);
		var prefix = A2(_elm_community$intdict$IntDict$lcp, lp.prefixBits, modifiedRightPrefix);
		return _elm_lang$core$Native_Utils.eq(lp, rp) ? _elm_community$intdict$IntDict$SamePrefix : (_elm_lang$core$Native_Utils.eq(prefix, lp) ? A2(
			_elm_community$intdict$IntDict$Parent,
			_elm_community$intdict$IntDict$Left,
			A2(childEdge, l.prefix, r)) : (_elm_lang$core$Native_Utils.eq(prefix, rp) ? A2(
			_elm_community$intdict$IntDict$Parent,
			_elm_community$intdict$IntDict$Right,
			A2(childEdge, r.prefix, l)) : A2(
			_elm_community$intdict$IntDict$Disjunct,
			prefix,
			A2(childEdge, prefix, l))));
	});
var _elm_community$intdict$IntDict$uniteWith = F3(
	function (merger, l, r) {
		var mergeWith = F3(
			function (key, left, right) {
				var _p36 = {ctor: '_Tuple2', _0: left, _1: right};
				if (_p36._0.ctor === 'Just') {
					if (_p36._1.ctor === 'Just') {
						return _elm_lang$core$Maybe$Just(
							A3(merger, key, _p36._0._0, _p36._1._0));
					} else {
						return left;
					}
				} else {
					if (_p36._1.ctor === 'Just') {
						return right;
					} else {
						return _elm_lang$core$Native_Utils.crashCase(
							'IntDict',
							{
								start: {line: 427, column: 7},
								end: {line: 432, column: 144}
							},
							_p36)('IntDict.uniteWith: mergeWith was called with 2 Nothings. This is a bug in the implementation, please file a bug report!');
					}
				}
			});
		var _p38 = {ctor: '_Tuple2', _0: l, _1: r};
		_v28_2:
		do {
			_v28_1:
			do {
				switch (_p38._0.ctor) {
					case 'Empty':
						return r;
					case 'Leaf':
						switch (_p38._1.ctor) {
							case 'Empty':
								break _v28_1;
							case 'Leaf':
								break _v28_2;
							default:
								break _v28_2;
						}
					default:
						switch (_p38._1.ctor) {
							case 'Empty':
								break _v28_1;
							case 'Leaf':
								var _p40 = _p38._1._0;
								return A3(
									_elm_community$intdict$IntDict$update,
									_p40.key,
									function (l_) {
										return A3(
											mergeWith,
											_p40.key,
											l_,
											_elm_lang$core$Maybe$Just(_p40.value));
									},
									l);
							default:
								var _p43 = _p38._1._0;
								var _p42 = _p38._0._0;
								var _p41 = A2(_elm_community$intdict$IntDict$determineBranchRelation, _p42, _p43);
								switch (_p41.ctor) {
									case 'SamePrefix':
										return A3(
											_elm_community$intdict$IntDict$inner,
											_p42.prefix,
											A3(_elm_community$intdict$IntDict$uniteWith, merger, _p42.left, _p43.left),
											A3(_elm_community$intdict$IntDict$uniteWith, merger, _p42.right, _p43.right));
									case 'Parent':
										if (_p41._0.ctor === 'Left') {
											if (_p41._1.ctor === 'Right') {
												return A3(
													_elm_community$intdict$IntDict$inner,
													_p42.prefix,
													_p42.left,
													A3(_elm_community$intdict$IntDict$uniteWith, merger, _p42.right, r));
											} else {
												return A3(
													_elm_community$intdict$IntDict$inner,
													_p42.prefix,
													A3(_elm_community$intdict$IntDict$uniteWith, merger, _p42.left, r),
													_p42.right);
											}
										} else {
											if (_p41._1.ctor === 'Right') {
												return A3(
													_elm_community$intdict$IntDict$inner,
													_p43.prefix,
													_p43.left,
													A3(_elm_community$intdict$IntDict$uniteWith, merger, l, _p43.right));
											} else {
												return A3(
													_elm_community$intdict$IntDict$inner,
													_p43.prefix,
													A3(_elm_community$intdict$IntDict$uniteWith, merger, l, _p43.left),
													_p43.right);
											}
										}
									default:
										if (_p41._1.ctor === 'Left') {
											return A3(_elm_community$intdict$IntDict$inner, _p41._0, l, r);
										} else {
											return A3(_elm_community$intdict$IntDict$inner, _p41._0, r, l);
										}
								}
						}
				}
			} while(false);
			return l;
		} while(false);
		var _p39 = _p38._0._0;
		return A3(
			_elm_community$intdict$IntDict$update,
			_p39.key,
			function (r_) {
				return A3(
					mergeWith,
					_p39.key,
					_elm_lang$core$Maybe$Just(_p39.value),
					r_);
			},
			r);
	});
var _elm_community$intdict$IntDict$union = _elm_community$intdict$IntDict$uniteWith(
	F3(
		function (key, old, $new) {
			return old;
		}));
var _elm_community$intdict$IntDict$intersect = F2(
	function (l, r) {
		intersect:
		while (true) {
			var _p44 = {ctor: '_Tuple2', _0: l, _1: r};
			_v30_2:
			do {
				_v30_1:
				do {
					switch (_p44._0.ctor) {
						case 'Empty':
							return _elm_community$intdict$IntDict$Empty;
						case 'Leaf':
							switch (_p44._1.ctor) {
								case 'Empty':
									break _v30_1;
								case 'Leaf':
									break _v30_2;
								default:
									break _v30_2;
							}
						default:
							switch (_p44._1.ctor) {
								case 'Empty':
									break _v30_1;
								case 'Leaf':
									var _p46 = _p44._1._0;
									var _p45 = A2(_elm_community$intdict$IntDict$get, _p46.key, l);
									if (_p45.ctor === 'Just') {
										return A2(_elm_community$intdict$IntDict$leaf, _p46.key, _p45._0);
									} else {
										return _elm_community$intdict$IntDict$Empty;
									}
								default:
									var _p49 = _p44._1._0;
									var _p48 = _p44._0._0;
									var _p47 = A2(_elm_community$intdict$IntDict$determineBranchRelation, _p48, _p49);
									switch (_p47.ctor) {
										case 'SamePrefix':
											return A3(
												_elm_community$intdict$IntDict$inner,
												_p48.prefix,
												A2(_elm_community$intdict$IntDict$intersect, _p48.left, _p49.left),
												A2(_elm_community$intdict$IntDict$intersect, _p48.right, _p49.right));
										case 'Parent':
											if (_p47._0.ctor === 'Left') {
												if (_p47._1.ctor === 'Right') {
													var _v33 = _p48.right,
														_v34 = r;
													l = _v33;
													r = _v34;
													continue intersect;
												} else {
													var _v35 = _p48.left,
														_v36 = r;
													l = _v35;
													r = _v36;
													continue intersect;
												}
											} else {
												if (_p47._1.ctor === 'Right') {
													var _v37 = l,
														_v38 = _p49.right;
													l = _v37;
													r = _v38;
													continue intersect;
												} else {
													var _v39 = l,
														_v40 = _p49.left;
													l = _v39;
													r = _v40;
													continue intersect;
												}
											}
										default:
											return _elm_community$intdict$IntDict$Empty;
									}
							}
					}
				} while(false);
				return _elm_community$intdict$IntDict$Empty;
			} while(false);
			return A2(_elm_community$intdict$IntDict$member, _p44._0._0.key, r) ? l : _elm_community$intdict$IntDict$Empty;
		}
	});
var _elm_community$intdict$IntDict$diff = F2(
	function (l, r) {
		diff:
		while (true) {
			var _p50 = {ctor: '_Tuple2', _0: l, _1: r};
			_v41_2:
			do {
				_v41_1:
				do {
					switch (_p50._0.ctor) {
						case 'Empty':
							return _elm_community$intdict$IntDict$Empty;
						case 'Leaf':
							switch (_p50._1.ctor) {
								case 'Empty':
									break _v41_1;
								case 'Leaf':
									break _v41_2;
								default:
									break _v41_2;
							}
						default:
							switch (_p50._1.ctor) {
								case 'Empty':
									break _v41_1;
								case 'Leaf':
									return A2(_elm_community$intdict$IntDict$remove, _p50._1._0.key, l);
								default:
									var _p53 = _p50._1._0;
									var _p52 = _p50._0._0;
									var _p51 = A2(_elm_community$intdict$IntDict$determineBranchRelation, _p52, _p53);
									switch (_p51.ctor) {
										case 'SamePrefix':
											return A3(
												_elm_community$intdict$IntDict$inner,
												_p52.prefix,
												A2(_elm_community$intdict$IntDict$diff, _p52.left, _p53.left),
												A2(_elm_community$intdict$IntDict$diff, _p52.right, _p53.right));
										case 'Parent':
											if (_p51._0.ctor === 'Left') {
												if (_p51._1.ctor === 'Left') {
													return A3(
														_elm_community$intdict$IntDict$inner,
														_p52.prefix,
														A2(_elm_community$intdict$IntDict$diff, _p52.left, r),
														_p52.right);
												} else {
													return A3(
														_elm_community$intdict$IntDict$inner,
														_p52.prefix,
														_p52.left,
														A2(_elm_community$intdict$IntDict$diff, _p52.right, r));
												}
											} else {
												if (_p51._1.ctor === 'Left') {
													var _v43 = l,
														_v44 = _p53.left;
													l = _v43;
													r = _v44;
													continue diff;
												} else {
													var _v45 = l,
														_v46 = _p53.right;
													l = _v45;
													r = _v46;
													continue diff;
												}
											}
										default:
											return l;
									}
							}
					}
				} while(false);
				return l;
			} while(false);
			return A2(_elm_community$intdict$IntDict$member, _p50._0._0.key, r) ? _elm_community$intdict$IntDict$Empty : l;
		}
	});
var _elm_community$intdict$IntDict$merge = F6(
	function (left, both, right, l, r, acc) {
		var m = A3(_elm_community$intdict$IntDict$merge, left, both, right);
		var _p54 = {ctor: '_Tuple2', _0: l, _1: r};
		_v47_2:
		do {
			_v47_1:
			do {
				switch (_p54._0.ctor) {
					case 'Empty':
						return A3(_elm_community$intdict$IntDict$foldl, right, acc, r);
					case 'Leaf':
						switch (_p54._1.ctor) {
							case 'Empty':
								break _v47_1;
							case 'Leaf':
								break _v47_2;
							default:
								break _v47_2;
						}
					default:
						switch (_p54._1.ctor) {
							case 'Empty':
								break _v47_1;
							case 'Leaf':
								var _p58 = _p54._1._0;
								var _p57 = A2(_elm_community$intdict$IntDict$get, _p58.key, l);
								if (_p57.ctor === 'Nothing') {
									return A3(
										m,
										l,
										_elm_community$intdict$IntDict$Empty,
										A3(right, _p58.key, _p58.value, acc));
								} else {
									return A3(
										m,
										A2(_elm_community$intdict$IntDict$remove, _p58.key, l),
										_elm_community$intdict$IntDict$Empty,
										A4(both, _p58.key, _p57._0, _p58.value, acc));
								}
							default:
								var _p61 = _p54._1._0;
								var _p60 = _p54._0._0;
								var _p59 = A2(_elm_community$intdict$IntDict$determineBranchRelation, _p60, _p61);
								switch (_p59.ctor) {
									case 'SamePrefix':
										return A3(
											m,
											_p60.right,
											_p61.right,
											A3(m, _p60.left, _p61.left, acc));
									case 'Parent':
										if (_p59._0.ctor === 'Left') {
											if (_p59._1.ctor === 'Left') {
												return A3(
													m,
													_p60.right,
													_elm_community$intdict$IntDict$Empty,
													A3(m, _p60.left, r, acc));
											} else {
												return A3(
													m,
													_p60.right,
													r,
													A3(m, _p60.left, _elm_community$intdict$IntDict$Empty, acc));
											}
										} else {
											if (_p59._1.ctor === 'Left') {
												return A3(
													m,
													_elm_community$intdict$IntDict$Empty,
													_p61.right,
													A3(m, l, _p61.left, acc));
											} else {
												return A3(
													m,
													l,
													_p61.right,
													A3(m, _elm_community$intdict$IntDict$Empty, _p61.left, acc));
											}
										}
									default:
										if (_p59._1.ctor === 'Left') {
											return A3(
												m,
												_elm_community$intdict$IntDict$Empty,
												r,
												A3(m, l, _elm_community$intdict$IntDict$Empty, acc));
										} else {
											return A3(
												m,
												l,
												_elm_community$intdict$IntDict$Empty,
												A3(m, _elm_community$intdict$IntDict$Empty, r, acc));
										}
								}
						}
				}
			} while(false);
			return A3(_elm_community$intdict$IntDict$foldl, left, acc, l);
		} while(false);
		var _p56 = _p54._0._0;
		var _p55 = A2(_elm_community$intdict$IntDict$get, _p56.key, r);
		if (_p55.ctor === 'Nothing') {
			return A3(
				m,
				_elm_community$intdict$IntDict$Empty,
				r,
				A3(left, _p56.key, _p56.value, acc));
		} else {
			return A3(
				m,
				_elm_community$intdict$IntDict$Empty,
				A2(_elm_community$intdict$IntDict$remove, _p56.key, r),
				A4(both, _p56.key, _p56.value, _p55._0, acc));
		}
	});

var _elm_community$graph$Graph$ignorePath = F4(
	function (visit, path, _p0, acc) {
		var _p1 = path;
		if (_p1.ctor === '[]') {
			return _elm_lang$core$Native_Utils.crashCase(
				'Graph',
				{
					start: {line: 880, column: 3},
					end: {line: 884, column: 20}
				},
				_p1)('Graph.ignorePath: No algorithm should ever pass an empty path into this BfsNodeVisitor.');
		} else {
			return A2(visit, _p1._0, acc);
		}
	});
var _elm_community$graph$Graph$onFinish = F3(
	function (visitor, ctx, acc) {
		return {
			ctor: '_Tuple2',
			_0: acc,
			_1: visitor(ctx)
		};
	});
var _elm_community$graph$Graph$onDiscovery = F3(
	function (visitor, ctx, acc) {
		return {
			ctor: '_Tuple2',
			_0: A2(visitor, ctx, acc),
			_1: _elm_lang$core$Basics$identity
		};
	});
var _elm_community$graph$Graph$alongIncomingEdges = function (ctx) {
	return _elm_community$intdict$IntDict$keys(ctx.incoming);
};
var _elm_community$graph$Graph$alongOutgoingEdges = function (ctx) {
	return _elm_community$intdict$IntDict$keys(ctx.outgoing);
};
var _elm_community$graph$Graph$applyEdgeDiff = F3(
	function (nodeId, diff, graphRep) {
		var updateOutgoingEdge = F2(
			function (upd, node) {
				return _elm_lang$core$Native_Utils.update(
					node,
					{
						outgoing: A3(_elm_community$intdict$IntDict$update, nodeId, upd, node.outgoing)
					});
			});
		var updateIncomingEdge = F2(
			function (upd, node) {
				return _elm_lang$core$Native_Utils.update(
					node,
					{
						incoming: A3(_elm_community$intdict$IntDict$update, nodeId, upd, node.incoming)
					});
			});
		var edgeUpdateToMaybe = function (edgeUpdate) {
			var _p3 = edgeUpdate;
			if (_p3.ctor === 'Insert') {
				return _elm_lang$core$Maybe$Just(_p3._0);
			} else {
				return _elm_lang$core$Maybe$Nothing;
			}
		};
		var updateAdjacency = F3(
			function (updateEdge, updatedId, edgeUpdate) {
				var updateLbl = updateEdge(
					_elm_lang$core$Basics$always(
						edgeUpdateToMaybe(edgeUpdate)));
				return A2(
					_elm_community$intdict$IntDict$update,
					updatedId,
					_elm_lang$core$Maybe$map(updateLbl));
			});
		var flippedFoldl = F3(
			function (f, dict, acc) {
				return A3(_elm_community$intdict$IntDict$foldl, f, acc, dict);
			});
		return A3(
			flippedFoldl,
			updateAdjacency(updateOutgoingEdge),
			diff.outgoing,
			A3(
				flippedFoldl,
				updateAdjacency(updateIncomingEdge),
				diff.incoming,
				graphRep));
	});
var _elm_community$graph$Graph$emptyDiff = {incoming: _elm_community$intdict$IntDict$empty, outgoing: _elm_community$intdict$IntDict$empty};
var _elm_community$graph$Graph$unGraph = function (graph) {
	var _p4 = graph;
	return _p4._0;
};
var _elm_community$graph$Graph$size = function (_p5) {
	return _elm_community$intdict$IntDict$size(
		_elm_community$graph$Graph$unGraph(_p5));
};
var _elm_community$graph$Graph$member = function (nodeId) {
	return function (_p6) {
		return A2(
			_elm_community$intdict$IntDict$member,
			nodeId,
			_elm_community$graph$Graph$unGraph(_p6));
	};
};
var _elm_community$graph$Graph$get = function (nodeId) {
	return function (_p7) {
		return A2(
			_elm_community$intdict$IntDict$get,
			nodeId,
			_elm_community$graph$Graph$unGraph(_p7));
	};
};
var _elm_community$graph$Graph$nodeIdRange = function (graph) {
	return A2(
		_elm_lang$core$Maybe$andThen,
		function (_p8) {
			var _p9 = _p8;
			return A2(
				_elm_lang$core$Maybe$andThen,
				function (_p10) {
					var _p11 = _p10;
					return _elm_lang$core$Maybe$Just(
						{ctor: '_Tuple2', _0: _p9._0, _1: _p11._0});
				},
				_elm_community$intdict$IntDict$findMax(
					_elm_community$graph$Graph$unGraph(graph)));
		},
		_elm_community$intdict$IntDict$findMin(
			_elm_community$graph$Graph$unGraph(graph)));
};
var _elm_community$graph$Graph$nodes = function (_p12) {
	return A2(
		_elm_lang$core$List$map,
		function (_) {
			return _.node;
		},
		_elm_community$intdict$IntDict$values(
			_elm_community$graph$Graph$unGraph(_p12)));
};
var _elm_community$graph$Graph$nodeIds = function (_p13) {
	return _elm_community$intdict$IntDict$keys(
		_elm_community$graph$Graph$unGraph(_p13));
};
var _elm_community$graph$Graph$edges = function (graph) {
	var flippedFoldl = F3(
		function (f, dict, list) {
			return A3(_elm_community$intdict$IntDict$foldl, f, list, dict);
		});
	var prependEdges = F2(
		function (node1, ctx) {
			return A2(
				flippedFoldl,
				F2(
					function (node2, e) {
						return F2(
							function (x, y) {
								return {ctor: '::', _0: x, _1: y};
							})(
							{to: node2, from: node1, label: e});
					}),
				ctx.outgoing);
		});
	return A3(
		flippedFoldl,
		prependEdges,
		_elm_community$graph$Graph$unGraph(graph),
		{ctor: '[]'});
};
var _elm_community$graph$Graph$toString = function (graph) {
	var edgeList = _elm_community$graph$Graph$edges(graph);
	var nodeList = _elm_community$graph$Graph$nodes(graph);
	return A2(
		_elm_lang$core$Basics_ops['++'],
		'Graph.fromNodesAndEdges ',
		A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Basics$toString(nodeList),
			A2(
				_elm_lang$core$Basics_ops['++'],
				' ',
				_elm_lang$core$Basics$toString(edgeList))));
};
var _elm_community$graph$Graph$Node = F2(
	function (a, b) {
		return {id: a, label: b};
	});
var _elm_community$graph$Graph$Edge = F3(
	function (a, b, c) {
		return {from: a, to: b, label: c};
	});
var _elm_community$graph$Graph$NodeContext = F3(
	function (a, b, c) {
		return {node: a, incoming: b, outgoing: c};
	});
var _elm_community$graph$Graph$EdgeDiff = F2(
	function (a, b) {
		return {incoming: a, outgoing: b};
	});
var _elm_community$graph$Graph$Graph = function (a) {
	return {ctor: 'Graph', _0: a};
};
var _elm_community$graph$Graph$empty = _elm_community$graph$Graph$Graph(_elm_community$intdict$IntDict$empty);
var _elm_community$graph$Graph$isEmpty = function (graph) {
	return _elm_lang$core$Native_Utils.eq(graph, _elm_community$graph$Graph$empty);
};
var _elm_community$graph$Graph$fromNodesAndEdges = F2(
	function (nodes, edges) {
		var addEdge = F2(
			function (edge, rep) {
				var updateIncoming = function (ctx) {
					return _elm_lang$core$Native_Utils.update(
						ctx,
						{
							incoming: A3(_elm_community$intdict$IntDict$insert, edge.from, edge.label, ctx.incoming)
						});
				};
				var updateOutgoing = function (ctx) {
					return _elm_lang$core$Native_Utils.update(
						ctx,
						{
							outgoing: A3(_elm_community$intdict$IntDict$insert, edge.to, edge.label, ctx.outgoing)
						});
				};
				return A3(
					_elm_community$intdict$IntDict$update,
					edge.to,
					_elm_lang$core$Maybe$map(updateIncoming),
					A3(
						_elm_community$intdict$IntDict$update,
						edge.from,
						_elm_lang$core$Maybe$map(updateOutgoing),
						rep));
			});
		var nodeRep = A3(
			_elm_lang$core$List$foldl,
			function (n) {
				return A2(
					_elm_community$intdict$IntDict$insert,
					n.id,
					A3(_elm_community$graph$Graph$NodeContext, n, _elm_community$intdict$IntDict$empty, _elm_community$intdict$IntDict$empty));
			},
			_elm_community$intdict$IntDict$empty,
			nodes);
		return _elm_community$graph$Graph$Graph(
			A3(_elm_lang$core$List$foldl, addEdge, nodeRep, edges));
	});
var _elm_community$graph$Graph$fromNodeLabelsAndEdgePairs = F2(
	function (labels, edgePairs) {
		var edges = A2(
			_elm_lang$core$List$map,
			function (_p14) {
				var _p15 = _p14;
				return A3(
					_elm_community$graph$Graph$Edge,
					_p15._0,
					_p15._1,
					{ctor: '_Tuple0'});
			},
			edgePairs);
		var nodes = _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldl,
				F2(
					function (lbl, _p16) {
						var _p17 = _p16;
						var _p18 = _p17._0;
						return {
							ctor: '_Tuple2',
							_0: _p18 + 1,
							_1: {
								ctor: '::',
								_0: A2(_elm_community$graph$Graph$Node, _p18, lbl),
								_1: _p17._1
							}
						};
					}),
				{
					ctor: '_Tuple2',
					_0: 0,
					_1: {ctor: '[]'}
				},
				labels));
		return A2(_elm_community$graph$Graph$fromNodesAndEdges, nodes, edges);
	});
var _elm_community$graph$Graph$symmetricClosure = function (edgeMerger) {
	var orderedEdgeMerger = F4(
		function (from, to, outgoing, incoming) {
			return (_elm_lang$core$Native_Utils.cmp(from, to) < 1) ? A4(edgeMerger, from, to, outgoing, incoming) : A4(edgeMerger, to, from, incoming, outgoing);
		});
	var updateContext = F2(
		function (nodeId, ctx) {
			var edges = A3(
				_elm_community$intdict$IntDict$uniteWith,
				orderedEdgeMerger(nodeId),
				ctx.outgoing,
				ctx.incoming);
			return _elm_lang$core$Native_Utils.update(
				ctx,
				{outgoing: edges, incoming: edges});
		});
	return function (_p19) {
		return _elm_community$graph$Graph$Graph(
			A2(
				_elm_community$intdict$IntDict$map,
				updateContext,
				_elm_community$graph$Graph$unGraph(_p19)));
	};
};
var _elm_community$graph$Graph$reverseEdges = function () {
	var updateContext = F2(
		function (nodeId, ctx) {
			return _elm_lang$core$Native_Utils.update(
				ctx,
				{outgoing: ctx.incoming, incoming: ctx.outgoing});
		});
	return function (_p20) {
		return _elm_community$graph$Graph$Graph(
			A2(
				_elm_community$intdict$IntDict$map,
				updateContext,
				_elm_community$graph$Graph$unGraph(_p20)));
	};
}();
var _elm_community$graph$Graph$Remove = function (a) {
	return {ctor: 'Remove', _0: a};
};
var _elm_community$graph$Graph$Insert = function (a) {
	return {ctor: 'Insert', _0: a};
};
var _elm_community$graph$Graph$computeEdgeDiff = F2(
	function (old, $new) {
		var collectUpdates = F3(
			function (edgeUpdate, updatedId, label) {
				var replaceUpdate = function (old) {
					var _p21 = {
						ctor: '_Tuple2',
						_0: old,
						_1: edgeUpdate(label)
					};
					if (_p21._0.ctor === 'Just') {
						if (_p21._0._0.ctor === 'Remove') {
							if (_p21._1.ctor === 'Insert') {
								var _p22 = _p21._1._0;
								return _elm_lang$core$Native_Utils.eq(_p21._0._0._0, _p22) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
									_elm_community$graph$Graph$Insert(_p22));
							} else {
								return _elm_lang$core$Native_Utils.crashCase(
									'Graph',
									{
										start: {line: 189, column: 11},
										end: {line: 199, column: 22}
									},
									_p21)('Graph.computeEdgeDiff: Collected two removals for the same edge. This is an error in the implementation of Graph and you should file a bug report!');
							}
						} else {
							return _elm_lang$core$Native_Utils.crashCase(
								'Graph',
								{
									start: {line: 189, column: 11},
									end: {line: 199, column: 22}
								},
								_p21)('Graph.computeEdgeDiff: Collected inserts before removals. This is an error in the implementation of Graph and you should file a bug report!');
						}
					} else {
						return _elm_lang$core$Maybe$Just(_p21._1);
					}
				};
				return A2(_elm_community$intdict$IntDict$update, updatedId, replaceUpdate);
			});
		var collect = F3(
			function (edgeUpdate, adj, updates) {
				return A3(
					_elm_community$intdict$IntDict$foldl,
					collectUpdates(edgeUpdate),
					updates,
					adj);
			});
		var _p25 = {ctor: '_Tuple2', _0: old, _1: $new};
		if (_p25._0.ctor === 'Nothing') {
			if (_p25._1.ctor === 'Nothing') {
				return _elm_community$graph$Graph$emptyDiff;
			} else {
				var _p27 = _p25._1._0;
				return {
					outgoing: A3(collect, _elm_community$graph$Graph$Insert, _p27.incoming, _elm_community$intdict$IntDict$empty),
					incoming: A3(collect, _elm_community$graph$Graph$Insert, _p27.outgoing, _elm_community$intdict$IntDict$empty)
				};
			}
		} else {
			if (_p25._1.ctor === 'Nothing') {
				var _p26 = _p25._0._0;
				return {
					outgoing: A3(collect, _elm_community$graph$Graph$Remove, _p26.incoming, _elm_community$intdict$IntDict$empty),
					incoming: A3(collect, _elm_community$graph$Graph$Remove, _p26.outgoing, _elm_community$intdict$IntDict$empty)
				};
			} else {
				var _p29 = _p25._0._0;
				var _p28 = _p25._1._0;
				return _elm_lang$core$Native_Utils.eq(_p29, _p28) ? _elm_community$graph$Graph$emptyDiff : {
					outgoing: A3(
						collect,
						_elm_community$graph$Graph$Insert,
						_p28.incoming,
						A3(collect, _elm_community$graph$Graph$Remove, _p29.incoming, _elm_community$intdict$IntDict$empty)),
					incoming: A3(
						collect,
						_elm_community$graph$Graph$Insert,
						_p28.outgoing,
						A3(collect, _elm_community$graph$Graph$Remove, _p29.outgoing, _elm_community$intdict$IntDict$empty))
				};
			}
		}
	});
var _elm_community$graph$Graph$update = F2(
	function (nodeId, updater) {
		var wrappedUpdater = function (rep) {
			var filterInvalidEdges = function (ctx) {
				return _elm_community$intdict$IntDict$filter(
					F2(
						function (id, _p30) {
							return _elm_lang$core$Native_Utils.eq(id, ctx.node.id) || A2(_elm_community$intdict$IntDict$member, id, rep);
						}));
			};
			var cleanUpEdges = function (ctx) {
				return _elm_lang$core$Native_Utils.update(
					ctx,
					{
						incoming: A2(filterInvalidEdges, ctx, ctx.incoming),
						outgoing: A2(filterInvalidEdges, ctx, ctx.outgoing)
					});
			};
			var old = A2(_elm_community$intdict$IntDict$get, nodeId, rep);
			var $new = A2(
				_elm_lang$core$Maybe$map,
				cleanUpEdges,
				updater(old));
			var diff = A2(_elm_community$graph$Graph$computeEdgeDiff, old, $new);
			return A3(
				_elm_community$intdict$IntDict$update,
				nodeId,
				_elm_lang$core$Basics$always($new),
				A3(_elm_community$graph$Graph$applyEdgeDiff, nodeId, diff, rep));
		};
		return function (_p31) {
			return _elm_community$graph$Graph$Graph(
				wrappedUpdater(
					_elm_community$graph$Graph$unGraph(_p31)));
		};
	});
var _elm_community$graph$Graph$insert = F2(
	function (nodeContext, graph) {
		return A3(
			_elm_community$graph$Graph$update,
			nodeContext.node.id,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(nodeContext)),
			graph);
	});
var _elm_community$graph$Graph$inducedSubgraph = F2(
	function (nodeIds, graph) {
		var insertContextById = F2(
			function (nodeId, acc) {
				var _p32 = A2(_elm_community$graph$Graph$get, nodeId, graph);
				if (_p32.ctor === 'Just') {
					return A2(_elm_community$graph$Graph$insert, _p32._0, acc);
				} else {
					return acc;
				}
			});
		return A3(_elm_lang$core$List$foldl, insertContextById, _elm_community$graph$Graph$empty, nodeIds);
	});
var _elm_community$graph$Graph$remove = F2(
	function (nodeId, graph) {
		return A3(
			_elm_community$graph$Graph$update,
			nodeId,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			graph);
	});
var _elm_community$graph$Graph$fold = F3(
	function (f, acc, graph) {
		var go = F2(
			function (acc, graph1) {
				go:
				while (true) {
					var maybeContext = A2(
						_elm_lang$core$Maybe$andThen,
						function (id) {
							return A2(_elm_community$graph$Graph$get, id, graph);
						},
						A2(
							_elm_lang$core$Maybe$map,
							_elm_lang$core$Tuple$first,
							_elm_community$graph$Graph$nodeIdRange(graph1)));
					var _p33 = maybeContext;
					if (_p33.ctor === 'Just') {
						var _p34 = _p33._0;
						var _v11 = A2(f, _p34, acc),
							_v12 = A2(_elm_community$graph$Graph$remove, _p34.node.id, graph1);
						acc = _v11;
						graph1 = _v12;
						continue go;
					} else {
						return acc;
					}
				}
			});
		return A2(go, acc, graph);
	});
var _elm_community$graph$Graph$mapContexts = function (f) {
	return A2(
		_elm_community$graph$Graph$fold,
		function (ctx) {
			return _elm_community$graph$Graph$insert(
				f(ctx));
		},
		_elm_community$graph$Graph$empty);
};
var _elm_community$graph$Graph$mapNodes = function (f) {
	return A2(
		_elm_community$graph$Graph$fold,
		function (ctx) {
			return _elm_community$graph$Graph$insert(
				_elm_lang$core$Native_Utils.update(
					ctx,
					{
						node: {
							id: ctx.node.id,
							label: f(ctx.node.label)
						}
					}));
		},
		_elm_community$graph$Graph$empty);
};
var _elm_community$graph$Graph$mapEdges = function (f) {
	return A2(
		_elm_community$graph$Graph$fold,
		function (ctx) {
			return _elm_community$graph$Graph$insert(
				_elm_lang$core$Native_Utils.update(
					ctx,
					{
						outgoing: A2(
							_elm_community$intdict$IntDict$map,
							F2(
								function (n, e) {
									return f(e);
								}),
							ctx.outgoing),
						incoming: A2(
							_elm_community$intdict$IntDict$map,
							F2(
								function (n, e) {
									return f(e);
								}),
							ctx.incoming)
					}));
		},
		_elm_community$graph$Graph$empty);
};
var _elm_community$graph$Graph$guidedDfs = F5(
	function (selectNeighbors, visitNode, seeds, acc, graph) {
		var go = F3(
			function (seeds, acc, graph) {
				go:
				while (true) {
					var _p35 = seeds;
					if (_p35.ctor === '[]') {
						return {ctor: '_Tuple2', _0: acc, _1: graph};
					} else {
						var _p41 = _p35._1;
						var _p40 = _p35._0;
						var _p36 = A2(_elm_community$graph$Graph$get, _p40, graph);
						if (_p36.ctor === 'Nothing') {
							var _v15 = _p41,
								_v16 = acc,
								_v17 = graph;
							seeds = _v15;
							acc = _v16;
							graph = _v17;
							continue go;
						} else {
							var _p39 = _p36._0;
							var _p37 = A2(visitNode, _p39, acc);
							var accAfterDiscovery = _p37._0;
							var finishNode = _p37._1;
							var _p38 = A3(
								go,
								selectNeighbors(_p39),
								accAfterDiscovery,
								A2(_elm_community$graph$Graph$remove, _p40, graph));
							var accBeforeFinish = _p38._0;
							var graph1 = _p38._1;
							var accAfterFinish = finishNode(accBeforeFinish);
							var _v18 = _p41,
								_v19 = accAfterFinish,
								_v20 = graph1;
							seeds = _v18;
							acc = _v19;
							graph = _v20;
							continue go;
						}
					}
				}
			});
		return A3(go, seeds, acc, graph);
	});
var _elm_community$graph$Graph$dfs = F3(
	function (visitNode, acc, graph) {
		return _elm_lang$core$Tuple$first(
			A5(
				_elm_community$graph$Graph$guidedDfs,
				_elm_community$graph$Graph$alongOutgoingEdges,
				visitNode,
				_elm_community$graph$Graph$nodeIds(graph),
				acc,
				graph));
	});
var _elm_community$graph$Graph$dfsForest = F2(
	function (seeds, graph) {
		var visitNode = F2(
			function (ctx, trees) {
				return {
					ctor: '_Tuple2',
					_0: {ctor: '[]'},
					_1: function (children) {
						return {
							ctor: '::',
							_0: A2(_elm_community$graph$Graph_Tree$inner, ctx, children),
							_1: trees
						};
					}
				};
			});
		return _elm_lang$core$List$reverse(
			_elm_lang$core$Tuple$first(
				A5(
					_elm_community$graph$Graph$guidedDfs,
					_elm_community$graph$Graph$alongOutgoingEdges,
					visitNode,
					seeds,
					{ctor: '[]'},
					graph)));
	});
var _elm_community$graph$Graph$dfsTree = F2(
	function (seed, graph) {
		var _p42 = A2(
			_elm_community$graph$Graph$dfsForest,
			{
				ctor: '::',
				_0: seed,
				_1: {ctor: '[]'}
			},
			graph);
		if (_p42.ctor === '[]') {
			return _elm_community$graph$Graph_Tree$empty;
		} else {
			if (_p42._1.ctor === '[]') {
				return _p42._0;
			} else {
				return _elm_lang$core$Native_Utils.crashCase(
					'Graph',
					{
						start: {line: 822, column: 3},
						end: {line: 828, column: 120}
					},
					_p42)('dfsTree: There can\'t be more than one DFS tree. This invariant is violated, please report this bug.');
			}
		}
	});
var _elm_community$graph$Graph$topologicalSort = function (graph) {
	return A2(
		_elm_lang$core$List$concatMap,
		_elm_community$graph$Graph_Tree$preOrderList,
		_elm_lang$core$List$reverse(
			A2(
				_elm_community$graph$Graph$dfsForest,
				_elm_community$graph$Graph$nodeIds(graph),
				graph)));
};
var _elm_community$graph$Graph$stronglyConnectedComponents = function (graph) {
	var timestamps = A3(
		_elm_community$graph$Graph$dfs,
		_elm_community$graph$Graph$onFinish(
			function (_p44) {
				return F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(
					function (_) {
						return _.id;
					}(
						function (_) {
							return _.node;
						}(_p44)));
			}),
		{ctor: '[]'},
		graph);
	var forest = A2(
		_elm_community$graph$Graph$dfsForest,
		timestamps,
		_elm_community$graph$Graph$reverseEdges(graph));
	var components = A2(
		_elm_lang$core$List$map,
		function (_p45) {
			return _elm_community$graph$Graph$reverseEdges(
				A3(
					_elm_lang$core$List$foldr,
					_elm_community$graph$Graph$insert,
					_elm_community$graph$Graph$empty,
					_elm_community$graph$Graph_Tree$preOrderList(_p45)));
		},
		forest);
	return components;
};
var _elm_community$graph$Graph$guidedBfs = F5(
	function (selectNeighbors, visitNode, seeds, acc, graph) {
		var enqueueMany = F4(
			function (distance, parentPath, nodeIds, queue) {
				return A3(
					_elm_lang$core$List$foldl,
					_avh4$elm_fifo$Fifo$insert,
					queue,
					A2(
						_elm_lang$core$List$map,
						function (id) {
							return {ctor: '_Tuple3', _0: id, _1: parentPath, _2: distance};
						},
						nodeIds));
			});
		var go = F3(
			function (seeds, acc, graph) {
				go:
				while (true) {
					var _p46 = _avh4$elm_fifo$Fifo$remove(seeds);
					if (_p46._0.ctor === 'Nothing') {
						return {ctor: '_Tuple2', _0: acc, _1: graph};
					} else {
						var _p51 = _p46._1;
						var _p50 = _p46._0._0._0;
						var _p49 = _p46._0._0._2;
						var _p47 = A2(_elm_community$graph$Graph$get, _p50, graph);
						if (_p47.ctor === 'Nothing') {
							var _v24 = _p51,
								_v25 = acc,
								_v26 = graph;
							seeds = _v24;
							acc = _v25;
							graph = _v26;
							continue go;
						} else {
							var _p48 = _p47._0;
							var path = {ctor: '::', _0: _p48, _1: _p46._0._0._1};
							var accAfterVisit = A3(visitNode, path, _p49, acc);
							var seeds2 = A4(
								enqueueMany,
								_p49 + 1,
								path,
								selectNeighbors(_p48),
								_p51);
							var _v27 = seeds2,
								_v28 = accAfterVisit,
								_v29 = A2(_elm_community$graph$Graph$remove, _p50, graph);
							seeds = _v27;
							acc = _v28;
							graph = _v29;
							continue go;
						}
					}
				}
			});
		return A3(
			go,
			A4(
				enqueueMany,
				0,
				{ctor: '[]'},
				seeds,
				_avh4$elm_fifo$Fifo$empty),
			acc,
			graph);
	});
var _elm_community$graph$Graph$bfs = F3(
	function (visitNode, acc, graph) {
		bfs:
		while (true) {
			var _p52 = _elm_community$graph$Graph$nodeIdRange(graph);
			if (_p52.ctor === 'Nothing') {
				return acc;
			} else {
				var _p53 = A5(
					_elm_community$graph$Graph$guidedBfs,
					_elm_community$graph$Graph$alongOutgoingEdges,
					visitNode,
					{
						ctor: '::',
						_0: _p52._0._0,
						_1: {ctor: '[]'}
					},
					acc,
					graph);
				var finalAcc = _p53._0;
				var restgraph1 = _p53._1;
				var _v31 = visitNode,
					_v32 = finalAcc,
					_v33 = restgraph1;
				visitNode = _v31;
				acc = _v32;
				graph = _v33;
				continue bfs;
			}
		}
	});
var _elm_community$graph$Graph$heightLevels = function (graph) {
	var subtract = F2(
		function (a, b) {
			return b - a;
		});
	var decrementAndNoteSources = F3(
		function (id, _p55, _p54) {
			var _p56 = _p54;
			var _p60 = _p56._0;
			var indegreesDec = A3(
				_elm_community$intdict$IntDict$update,
				id,
				_elm_lang$core$Maybe$map(
					subtract(1)),
				_p56._1);
			var _p57 = A2(_elm_community$intdict$IntDict$get, id, indegreesDec);
			if ((_p57.ctor === 'Just') && (_p57._0 === 0)) {
				var _p58 = A2(_elm_community$graph$Graph$get, id, graph);
				if (_p58.ctor === 'Just') {
					return {
						ctor: '_Tuple2',
						_0: {ctor: '::', _0: _p58._0, _1: _p60},
						_1: indegreesDec
					};
				} else {
					return _elm_lang$core$Native_Utils.crashCase(
						'Graph',
						{
							start: {line: 1001, column: 13},
							end: {line: 1003, column: 154}
						},
						_p58)('Graph.heightLevels: Could not get a node of a graph which should be there by invariants. Please file a bug report!');
				}
			} else {
				return {ctor: '_Tuple2', _0: _p60, _1: indegreesDec};
			}
		});
	var decrementIndegrees = F3(
		function (source, nextLevel, indegrees) {
			return A3(
				_elm_community$intdict$IntDict$foldl,
				decrementAndNoteSources,
				{ctor: '_Tuple2', _0: nextLevel, _1: indegrees},
				source.outgoing);
		});
	var go = F4(
		function (currentLevel, nextLevel, indegrees, graph) {
			var _p61 = {ctor: '_Tuple2', _0: currentLevel, _1: nextLevel};
			if (_p61._0.ctor === '[]') {
				if (_p61._1.ctor === '[]') {
					return {
						ctor: '::',
						_0: {ctor: '[]'},
						_1: {ctor: '[]'}
					};
				} else {
					return {
						ctor: '::',
						_0: {ctor: '[]'},
						_1: A4(
							go,
							nextLevel,
							{ctor: '[]'},
							indegrees,
							graph)
					};
				}
			} else {
				var _p65 = _p61._0._0;
				var _p62 = A3(decrementIndegrees, _p65, nextLevel, indegrees);
				var nextLevel1 = _p62._0;
				var indegrees1 = _p62._1;
				var _p63 = A4(
					go,
					_p61._0._1,
					nextLevel1,
					indegrees1,
					A2(_elm_community$graph$Graph$remove, _p65.node.id, graph));
				if (_p63.ctor === '[]') {
					return _elm_lang$core$Native_Utils.crashCase(
						'Graph',
						{
							start: {line: 1020, column: 13},
							end: {line: 1024, column: 44}
						},
						_p63)('Graph.heightLevels: Reached a branch which is impossible by invariants. Please file a bug report!');
				} else {
					return {
						ctor: '::',
						_0: {ctor: '::', _0: _p65, _1: _p63._0},
						_1: _p63._1
					};
				}
			}
		});
	var countIndegrees = A2(
		_elm_community$graph$Graph$fold,
		function (ctx) {
			return A2(
				_elm_community$intdict$IntDict$insert,
				ctx.node.id,
				_elm_community$intdict$IntDict$size(ctx.incoming));
		},
		_elm_community$intdict$IntDict$empty);
	var sources = A3(
		_elm_community$graph$Graph$fold,
		F2(
			function (ctx, acc) {
				return _elm_community$intdict$IntDict$isEmpty(ctx.incoming) ? {ctor: '::', _0: ctx, _1: acc} : acc;
			}),
		{ctor: '[]'},
		graph);
	return A4(
		go,
		sources,
		{ctor: '[]'},
		countIndegrees(graph),
		graph);
};

var _elm_lang$core$Set$foldr = F3(
	function (f, b, _p0) {
		var _p1 = _p0;
		return A3(
			_elm_lang$core$Dict$foldr,
			F3(
				function (k, _p2, b) {
					return A2(f, k, b);
				}),
			b,
			_p1._0);
	});
var _elm_lang$core$Set$foldl = F3(
	function (f, b, _p3) {
		var _p4 = _p3;
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, _p5, b) {
					return A2(f, k, b);
				}),
			b,
			_p4._0);
	});
var _elm_lang$core$Set$toList = function (_p6) {
	var _p7 = _p6;
	return _elm_lang$core$Dict$keys(_p7._0);
};
var _elm_lang$core$Set$size = function (_p8) {
	var _p9 = _p8;
	return _elm_lang$core$Dict$size(_p9._0);
};
var _elm_lang$core$Set$member = F2(
	function (k, _p10) {
		var _p11 = _p10;
		return A2(_elm_lang$core$Dict$member, k, _p11._0);
	});
var _elm_lang$core$Set$isEmpty = function (_p12) {
	var _p13 = _p12;
	return _elm_lang$core$Dict$isEmpty(_p13._0);
};
var _elm_lang$core$Set$Set_elm_builtin = function (a) {
	return {ctor: 'Set_elm_builtin', _0: a};
};
var _elm_lang$core$Set$empty = _elm_lang$core$Set$Set_elm_builtin(_elm_lang$core$Dict$empty);
var _elm_lang$core$Set$singleton = function (k) {
	return _elm_lang$core$Set$Set_elm_builtin(
		A2(
			_elm_lang$core$Dict$singleton,
			k,
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Set$insert = F2(
	function (k, _p14) {
		var _p15 = _p14;
		return _elm_lang$core$Set$Set_elm_builtin(
			A3(
				_elm_lang$core$Dict$insert,
				k,
				{ctor: '_Tuple0'},
				_p15._0));
	});
var _elm_lang$core$Set$fromList = function (xs) {
	return A3(_elm_lang$core$List$foldl, _elm_lang$core$Set$insert, _elm_lang$core$Set$empty, xs);
};
var _elm_lang$core$Set$map = F2(
	function (f, s) {
		return _elm_lang$core$Set$fromList(
			A2(
				_elm_lang$core$List$map,
				f,
				_elm_lang$core$Set$toList(s)));
	});
var _elm_lang$core$Set$remove = F2(
	function (k, _p16) {
		var _p17 = _p16;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$remove, k, _p17._0));
	});
var _elm_lang$core$Set$union = F2(
	function (_p19, _p18) {
		var _p20 = _p19;
		var _p21 = _p18;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$union, _p20._0, _p21._0));
	});
var _elm_lang$core$Set$intersect = F2(
	function (_p23, _p22) {
		var _p24 = _p23;
		var _p25 = _p22;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$intersect, _p24._0, _p25._0));
	});
var _elm_lang$core$Set$diff = F2(
	function (_p27, _p26) {
		var _p28 = _p27;
		var _p29 = _p26;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$diff, _p28._0, _p29._0));
	});
var _elm_lang$core$Set$filter = F2(
	function (p, _p30) {
		var _p31 = _p30;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(
				_elm_lang$core$Dict$filter,
				F2(
					function (k, _p32) {
						return p(k);
					}),
				_p31._0));
	});
var _elm_lang$core$Set$partition = F2(
	function (p, _p33) {
		var _p34 = _p33;
		var _p35 = A2(
			_elm_lang$core$Dict$partition,
			F2(
				function (k, _p36) {
					return p(k);
				}),
			_p34._0);
		var p1 = _p35._0;
		var p2 = _p35._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Set$Set_elm_builtin(p1),
			_1: _elm_lang$core$Set$Set_elm_builtin(p2)
		};
	});

var _elm_community$list_extra$List_Extra$greedyGroupsOfWithStep = F3(
	function (size, step, xs) {
		var okayXs = _elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$List$length(xs),
			0) > 0;
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		return (okayArgs && okayXs) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$greedyGroupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$groupsOfWithStep = F3(
	function (size, step, xs) {
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		var okayLength = _elm_lang$core$Native_Utils.eq(
			size,
			_elm_lang$core$List$length(group));
		return (okayArgs && okayLength) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$groupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$zip5 = _elm_lang$core$List$map5(
	F5(
		function (v0, v1, v2, v3, v4) {
			return {ctor: '_Tuple5', _0: v0, _1: v1, _2: v2, _3: v3, _4: v4};
		}));
var _elm_community$list_extra$List_Extra$zip4 = _elm_lang$core$List$map4(
	F4(
		function (v0, v1, v2, v3) {
			return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
		}));
var _elm_community$list_extra$List_Extra$zip3 = _elm_lang$core$List$map3(
	F3(
		function (v0, v1, v2) {
			return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
		}));
var _elm_community$list_extra$List_Extra$zip = _elm_lang$core$List$map2(
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}));
var _elm_community$list_extra$List_Extra$isPrefixOf = F2(
	function (prefix, xs) {
		var _p0 = {ctor: '_Tuple2', _0: prefix, _1: xs};
		if (_p0._0.ctor === '[]') {
			return true;
		} else {
			if (_p0._1.ctor === '[]') {
				return false;
			} else {
				return _elm_lang$core$Native_Utils.eq(_p0._0._0, _p0._1._0) && A2(_elm_community$list_extra$List_Extra$isPrefixOf, _p0._0._1, _p0._1._1);
			}
		}
	});
var _elm_community$list_extra$List_Extra$isSuffixOf = F2(
	function (suffix, xs) {
		return A2(
			_elm_community$list_extra$List_Extra$isPrefixOf,
			_elm_lang$core$List$reverse(suffix),
			_elm_lang$core$List$reverse(xs));
	});
var _elm_community$list_extra$List_Extra$selectSplit = function (xs) {
	var _p1 = xs;
	if (_p1.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p5 = _p1._1;
		var _p4 = _p1._0;
		return {
			ctor: '::',
			_0: {
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _p4,
				_2: _p5
			},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p2) {
					var _p3 = _p2;
					return {
						ctor: '_Tuple3',
						_0: {ctor: '::', _0: _p4, _1: _p3._0},
						_1: _p3._1,
						_2: _p3._2
					};
				},
				_elm_community$list_extra$List_Extra$selectSplit(_p5))
		};
	}
};
var _elm_community$list_extra$List_Extra$select = function (xs) {
	var _p6 = xs;
	if (_p6.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p10 = _p6._1;
		var _p9 = _p6._0;
		return {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: _p9, _1: _p10},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p7) {
					var _p8 = _p7;
					return {
						ctor: '_Tuple2',
						_0: _p8._0,
						_1: {ctor: '::', _0: _p9, _1: _p8._1}
					};
				},
				_elm_community$list_extra$List_Extra$select(_p10))
		};
	}
};
var _elm_community$list_extra$List_Extra$tailsHelp = F2(
	function (e, list) {
		var _p11 = list;
		if (_p11.ctor === '::') {
			var _p12 = _p11._0;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: e, _1: _p12},
				_1: {ctor: '::', _0: _p12, _1: _p11._1}
			};
		} else {
			return {ctor: '[]'};
		}
	});
var _elm_community$list_extra$List_Extra$tails = A2(
	_elm_lang$core$List$foldr,
	_elm_community$list_extra$List_Extra$tailsHelp,
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$isInfixOf = F2(
	function (infix, xs) {
		return A2(
			_elm_lang$core$List$any,
			_elm_community$list_extra$List_Extra$isPrefixOf(infix),
			_elm_community$list_extra$List_Extra$tails(xs));
	});
var _elm_community$list_extra$List_Extra$inits = A2(
	_elm_lang$core$List$foldr,
	F2(
		function (e, acc) {
			return {
				ctor: '::',
				_0: {ctor: '[]'},
				_1: A2(
					_elm_lang$core$List$map,
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						})(e),
					acc)
			};
		}),
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$groupWhileTransitively = F2(
	function (cmp, xs_) {
		var _p13 = xs_;
		if (_p13.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p13._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: {
						ctor: '::',
						_0: _p13._0,
						_1: {ctor: '[]'}
					},
					_1: {ctor: '[]'}
				};
			} else {
				var _p15 = _p13._0;
				var _p14 = A2(_elm_community$list_extra$List_Extra$groupWhileTransitively, cmp, _p13._1);
				if (_p14.ctor === '::') {
					return A2(cmp, _p15, _p13._1._0) ? {
						ctor: '::',
						_0: {ctor: '::', _0: _p15, _1: _p14._0},
						_1: _p14._1
					} : {
						ctor: '::',
						_0: {
							ctor: '::',
							_0: _p15,
							_1: {ctor: '[]'}
						},
						_1: _p14
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$stripPrefix = F2(
	function (prefix, xs) {
		var step = F2(
			function (e, m) {
				var _p16 = m;
				if (_p16.ctor === 'Nothing') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					if (_p16._0.ctor === '[]') {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						return _elm_lang$core$Native_Utils.eq(e, _p16._0._0) ? _elm_lang$core$Maybe$Just(_p16._0._1) : _elm_lang$core$Maybe$Nothing;
					}
				}
			});
		return A3(
			_elm_lang$core$List$foldl,
			step,
			_elm_lang$core$Maybe$Just(xs),
			prefix);
	});
var _elm_community$list_extra$List_Extra$dropWhileRight = function (p) {
	return A2(
		_elm_lang$core$List$foldr,
		F2(
			function (x, xs) {
				return (p(x) && _elm_lang$core$List$isEmpty(xs)) ? {ctor: '[]'} : {ctor: '::', _0: x, _1: xs};
			}),
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$takeWhileRight = function (p) {
	var step = F2(
		function (x, _p17) {
			var _p18 = _p17;
			var _p19 = _p18._0;
			return (p(x) && _p18._1) ? {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: x, _1: _p19},
				_1: true
			} : {ctor: '_Tuple2', _0: _p19, _1: false};
		});
	return function (_p20) {
		return _elm_lang$core$Tuple$first(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: {ctor: '[]'},
					_1: true
				},
				_p20));
	};
};
var _elm_community$list_extra$List_Extra$splitAt = F2(
	function (n, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_lang$core$List$take, n, xs),
			_1: A2(_elm_lang$core$List$drop, n, xs)
		};
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying_ = F3(
	function (listOflengths, list, accu) {
		groupsOfVarying_:
		while (true) {
			var _p21 = {ctor: '_Tuple2', _0: listOflengths, _1: list};
			if (((_p21.ctor === '_Tuple2') && (_p21._0.ctor === '::')) && (_p21._1.ctor === '::')) {
				var _p22 = A2(_elm_community$list_extra$List_Extra$splitAt, _p21._0._0, list);
				var head = _p22._0;
				var tail = _p22._1;
				var _v11 = _p21._0._1,
					_v12 = tail,
					_v13 = {ctor: '::', _0: head, _1: accu};
				listOflengths = _v11;
				list = _v12;
				accu = _v13;
				continue groupsOfVarying_;
			} else {
				return _elm_lang$core$List$reverse(accu);
			}
		}
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying = F2(
	function (listOflengths, list) {
		return A3(
			_elm_community$list_extra$List_Extra$groupsOfVarying_,
			listOflengths,
			list,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$unfoldr = F2(
	function (f, seed) {
		var _p23 = f(seed);
		if (_p23.ctor === 'Nothing') {
			return {ctor: '[]'};
		} else {
			return {
				ctor: '::',
				_0: _p23._0._0,
				_1: A2(_elm_community$list_extra$List_Extra$unfoldr, f, _p23._0._1)
			};
		}
	});
var _elm_community$list_extra$List_Extra$scanr1 = F2(
	function (f, xs_) {
		var _p24 = xs_;
		if (_p24.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p24._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: _p24._0,
					_1: {ctor: '[]'}
				};
			} else {
				var _p25 = A2(_elm_community$list_extra$List_Extra$scanr1, f, _p24._1);
				if (_p25.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, _p24._0, _p25._0),
						_1: _p25
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanr = F3(
	function (f, acc, xs_) {
		var _p26 = xs_;
		if (_p26.ctor === '[]') {
			return {
				ctor: '::',
				_0: acc,
				_1: {ctor: '[]'}
			};
		} else {
			var _p27 = A3(_elm_community$list_extra$List_Extra$scanr, f, acc, _p26._1);
			if (_p27.ctor === '::') {
				return {
					ctor: '::',
					_0: A2(f, _p26._0, _p27._0),
					_1: _p27
				};
			} else {
				return {ctor: '[]'};
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanl1 = F2(
	function (f, xs_) {
		var _p28 = xs_;
		if (_p28.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			return A3(_elm_lang$core$List$scanl, f, _p28._0, _p28._1);
		}
	});
var _elm_community$list_extra$List_Extra$indexedFoldr = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p29) {
				var _p30 = _p29;
				var _p31 = _p30._0;
				return {
					ctor: '_Tuple2',
					_0: _p31 - 1,
					_1: A3(func, _p31, x, _p30._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: _elm_lang$core$List$length(list) - 1,
					_1: acc
				},
				list));
	});
var _elm_community$list_extra$List_Extra$indexedFoldl = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p32) {
				var _p33 = _p32;
				var _p34 = _p33._0;
				return {
					ctor: '_Tuple2',
					_0: _p34 + 1,
					_1: A3(func, _p34, x, _p33._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldl,
				step,
				{ctor: '_Tuple2', _0: 0, _1: acc},
				list));
	});
var _elm_community$list_extra$List_Extra$foldr1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p35 = m;
						if (_p35.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, x, _p35._0);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldr, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$foldl1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p36 = m;
						if (_p36.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, _p36._0, x);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldl, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$interweaveHelp = F3(
	function (l1, l2, acc) {
		interweaveHelp:
		while (true) {
			var _p37 = {ctor: '_Tuple2', _0: l1, _1: l2};
			_v24_1:
			do {
				if (_p37._0.ctor === '::') {
					if (_p37._1.ctor === '::') {
						var _v25 = _p37._0._1,
							_v26 = _p37._1._1,
							_v27 = A2(
							_elm_lang$core$Basics_ops['++'],
							acc,
							{
								ctor: '::',
								_0: _p37._0._0,
								_1: {
									ctor: '::',
									_0: _p37._1._0,
									_1: {ctor: '[]'}
								}
							});
						l1 = _v25;
						l2 = _v26;
						acc = _v27;
						continue interweaveHelp;
					} else {
						break _v24_1;
					}
				} else {
					if (_p37._1.ctor === '[]') {
						break _v24_1;
					} else {
						return A2(_elm_lang$core$Basics_ops['++'], acc, _p37._1);
					}
				}
			} while(false);
			return A2(_elm_lang$core$Basics_ops['++'], acc, _p37._0);
		}
	});
var _elm_community$list_extra$List_Extra$interweave = F2(
	function (l1, l2) {
		return A3(
			_elm_community$list_extra$List_Extra$interweaveHelp,
			l1,
			l2,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$permutations = function (xs_) {
	var _p38 = xs_;
	if (_p38.ctor === '[]') {
		return {
			ctor: '::',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		};
	} else {
		var f = function (_p39) {
			var _p40 = _p39;
			return A2(
				_elm_lang$core$List$map,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(_p40._0),
				_elm_community$list_extra$List_Extra$permutations(_p40._1));
		};
		return A2(
			_elm_lang$core$List$concatMap,
			f,
			_elm_community$list_extra$List_Extra$select(_p38));
	}
};
var _elm_community$list_extra$List_Extra$isPermutationOf = F2(
	function (permut, xs) {
		return A2(
			_elm_lang$core$List$member,
			permut,
			_elm_community$list_extra$List_Extra$permutations(xs));
	});
var _elm_community$list_extra$List_Extra$subsequencesNonEmpty = function (xs) {
	var _p41 = xs;
	if (_p41.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p42 = _p41._0;
		var f = F2(
			function (ys, r) {
				return {
					ctor: '::',
					_0: ys,
					_1: {
						ctor: '::',
						_0: {ctor: '::', _0: _p42, _1: ys},
						_1: r
					}
				};
			});
		return {
			ctor: '::',
			_0: {
				ctor: '::',
				_0: _p42,
				_1: {ctor: '[]'}
			},
			_1: A3(
				_elm_lang$core$List$foldr,
				f,
				{ctor: '[]'},
				_elm_community$list_extra$List_Extra$subsequencesNonEmpty(_p41._1))
		};
	}
};
var _elm_community$list_extra$List_Extra$subsequences = function (xs) {
	return {
		ctor: '::',
		_0: {ctor: '[]'},
		_1: _elm_community$list_extra$List_Extra$subsequencesNonEmpty(xs)
	};
};
var _elm_community$list_extra$List_Extra$isSubsequenceOf = F2(
	function (subseq, xs) {
		return A2(
			_elm_lang$core$List$member,
			subseq,
			_elm_community$list_extra$List_Extra$subsequences(xs));
	});
var _elm_community$list_extra$List_Extra$transpose = function (ll) {
	transpose:
	while (true) {
		var _p43 = ll;
		if (_p43.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p43._0.ctor === '[]') {
				var _v32 = _p43._1;
				ll = _v32;
				continue transpose;
			} else {
				var _p44 = _p43._1;
				var tails = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$tail, _p44);
				var heads = A2(_elm_lang$core$List$filterMap, _elm_lang$core$List$head, _p44);
				return {
					ctor: '::',
					_0: {ctor: '::', _0: _p43._0._0, _1: heads},
					_1: _elm_community$list_extra$List_Extra$transpose(
						{ctor: '::', _0: _p43._0._1, _1: tails})
				};
			}
		}
	}
};
var _elm_community$list_extra$List_Extra$intercalate = function (xs) {
	return function (_p45) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$intersperse, xs, _p45));
	};
};
var _elm_community$list_extra$List_Extra$filterNot = F2(
	function (pred, list) {
		return A2(
			_elm_lang$core$List$filter,
			function (_p46) {
				return !pred(_p46);
			},
			list);
	});
var _elm_community$list_extra$List_Extra$removeAt = F2(
	function (index, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return l;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p47 = tail;
			if (_p47.ctor === 'Nothing') {
				return l;
			} else {
				return A2(_elm_lang$core$List$append, head, _p47._0);
			}
		}
	});
var _elm_community$list_extra$List_Extra$stableSortWith = F2(
	function (pred, list) {
		var predWithIndex = F2(
			function (_p49, _p48) {
				var _p50 = _p49;
				var _p51 = _p48;
				var result = A2(pred, _p50._0, _p51._0);
				var _p52 = result;
				if (_p52.ctor === 'EQ') {
					return A2(_elm_lang$core$Basics$compare, _p50._1, _p51._1);
				} else {
					return result;
				}
			});
		var listWithIndex = A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, a) {
					return {ctor: '_Tuple2', _0: a, _1: i};
				}),
			list);
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(_elm_lang$core$List$sortWith, predWithIndex, listWithIndex));
	});
var _elm_community$list_extra$List_Extra$setAt = F3(
	function (index, value, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p53 = tail;
			if (_p53.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return _elm_lang$core$Maybe$Just(
					A2(
						_elm_lang$core$List$append,
						head,
						{ctor: '::', _0: value, _1: _p53._0}));
			}
		}
	});
var _elm_community$list_extra$List_Extra$remove = F2(
	function (x, xs) {
		var _p54 = xs;
		if (_p54.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p56 = _p54._1;
			var _p55 = _p54._0;
			return _elm_lang$core$Native_Utils.eq(x, _p55) ? _p56 : {
				ctor: '::',
				_0: _p55,
				_1: A2(_elm_community$list_extra$List_Extra$remove, x, _p56)
			};
		}
	});
var _elm_community$list_extra$List_Extra$updateIfIndex = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, x) {
					return predicate(i) ? update(x) : x;
				}),
			list);
	});
var _elm_community$list_extra$List_Extra$updateAt = F3(
	function (index, update, list) {
		return ((_elm_lang$core$Native_Utils.cmp(index, 0) < 0) || (_elm_lang$core$Native_Utils.cmp(
			index,
			_elm_lang$core$List$length(list)) > -1)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
			A3(
				_elm_community$list_extra$List_Extra$updateIfIndex,
				F2(
					function (x, y) {
						return _elm_lang$core$Native_Utils.eq(x, y);
					})(index),
				update,
				list));
	});
var _elm_community$list_extra$List_Extra$updateIf = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$map,
			function (item) {
				return predicate(item) ? update(item) : item;
			},
			list);
	});
var _elm_community$list_extra$List_Extra$replaceIf = F3(
	function (predicate, replacement, list) {
		return A3(
			_elm_community$list_extra$List_Extra$updateIf,
			predicate,
			_elm_lang$core$Basics$always(replacement),
			list);
	});
var _elm_community$list_extra$List_Extra$findIndices = function (p) {
	return function (_p57) {
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(
				_elm_lang$core$List$filter,
				function (_p58) {
					var _p59 = _p58;
					return p(_p59._1);
				},
				A2(
					_elm_lang$core$List$indexedMap,
					F2(
						function (v0, v1) {
							return {ctor: '_Tuple2', _0: v0, _1: v1};
						}),
					_p57)));
	};
};
var _elm_community$list_extra$List_Extra$findIndex = function (p) {
	return function (_p60) {
		return _elm_lang$core$List$head(
			A2(_elm_community$list_extra$List_Extra$findIndices, p, _p60));
	};
};
var _elm_community$list_extra$List_Extra$splitWhen = F2(
	function (predicate, list) {
		return A2(
			_elm_lang$core$Maybe$map,
			function (i) {
				return A2(_elm_community$list_extra$List_Extra$splitAt, i, list);
			},
			A2(_elm_community$list_extra$List_Extra$findIndex, predicate, list));
	});
var _elm_community$list_extra$List_Extra$elemIndices = function (x) {
	return _elm_community$list_extra$List_Extra$findIndices(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$elemIndex = function (x) {
	return _elm_community$list_extra$List_Extra$findIndex(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			var _p61 = list;
			if (_p61.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p62 = _p61._0;
				if (predicate(_p62)) {
					return _elm_lang$core$Maybe$Just(_p62);
				} else {
					var _v41 = predicate,
						_v42 = _p61._1;
					predicate = _v41;
					list = _v42;
					continue find;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$notMember = function (x) {
	return function (_p63) {
		return !A2(_elm_lang$core$List$member, x, _p63);
	};
};
var _elm_community$list_extra$List_Extra$andThen = _elm_lang$core$List$concatMap;
var _elm_community$list_extra$List_Extra$lift2 = F3(
	function (f, la, lb) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return {
							ctor: '::',
							_0: A2(f, a, b),
							_1: {ctor: '[]'}
						};
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift3 = F4(
	function (f, la, lb, lc) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return {
									ctor: '::',
									_0: A3(f, a, b, c),
									_1: {ctor: '[]'}
								};
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift4 = F5(
	function (f, la, lb, lc, ld) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return A2(
									_elm_community$list_extra$List_Extra$andThen,
									function (d) {
										return {
											ctor: '::',
											_0: A4(f, a, b, c, d),
											_1: {ctor: '[]'}
										};
									},
									ld);
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$andMap = F2(
	function (l, fl) {
		return A3(
			_elm_lang$core$List$map2,
			F2(
				function (x, y) {
					return x(y);
				}),
			fl,
			l);
	});
var _elm_community$list_extra$List_Extra$uniqueHelp = F3(
	function (f, existing, remaining) {
		uniqueHelp:
		while (true) {
			var _p64 = remaining;
			if (_p64.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				var _p66 = _p64._1;
				var _p65 = _p64._0;
				var computedFirst = f(_p65);
				if (A2(_elm_lang$core$Set$member, computedFirst, existing)) {
					var _v44 = f,
						_v45 = existing,
						_v46 = _p66;
					f = _v44;
					existing = _v45;
					remaining = _v46;
					continue uniqueHelp;
				} else {
					return {
						ctor: '::',
						_0: _p65,
						_1: A3(
							_elm_community$list_extra$List_Extra$uniqueHelp,
							f,
							A2(_elm_lang$core$Set$insert, computedFirst, existing),
							_p66)
					};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$uniqueBy = F2(
	function (f, list) {
		return A3(_elm_community$list_extra$List_Extra$uniqueHelp, f, _elm_lang$core$Set$empty, list);
	});
var _elm_community$list_extra$List_Extra$allDifferentBy = F2(
	function (f, list) {
		return _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$List$length(list),
			_elm_lang$core$List$length(
				A2(_elm_community$list_extra$List_Extra$uniqueBy, f, list)));
	});
var _elm_community$list_extra$List_Extra$allDifferent = function (list) {
	return A2(_elm_community$list_extra$List_Extra$allDifferentBy, _elm_lang$core$Basics$identity, list);
};
var _elm_community$list_extra$List_Extra$unique = function (list) {
	return A3(_elm_community$list_extra$List_Extra$uniqueHelp, _elm_lang$core$Basics$identity, _elm_lang$core$Set$empty, list);
};
var _elm_community$list_extra$List_Extra$dropWhile = F2(
	function (predicate, list) {
		dropWhile:
		while (true) {
			var _p67 = list;
			if (_p67.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				if (predicate(_p67._0)) {
					var _v48 = predicate,
						_v49 = _p67._1;
					predicate = _v48;
					list = _v49;
					continue dropWhile;
				} else {
					return list;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$takeWhile = function (predicate) {
	var takeWhileMemo = F2(
		function (memo, list) {
			takeWhileMemo:
			while (true) {
				var _p68 = list;
				if (_p68.ctor === '[]') {
					return _elm_lang$core$List$reverse(memo);
				} else {
					var _p69 = _p68._0;
					if (predicate(_p69)) {
						var _v51 = {ctor: '::', _0: _p69, _1: memo},
							_v52 = _p68._1;
						memo = _v51;
						list = _v52;
						continue takeWhileMemo;
					} else {
						return _elm_lang$core$List$reverse(memo);
					}
				}
			}
		});
	return takeWhileMemo(
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$span = F2(
	function (p, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_community$list_extra$List_Extra$takeWhile, p, xs),
			_1: A2(_elm_community$list_extra$List_Extra$dropWhile, p, xs)
		};
	});
var _elm_community$list_extra$List_Extra$break = function (p) {
	return _elm_community$list_extra$List_Extra$span(
		function (_p70) {
			return !p(_p70);
		});
};
var _elm_community$list_extra$List_Extra$groupWhile = F2(
	function (eq, xs_) {
		var _p71 = xs_;
		if (_p71.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p73 = _p71._0;
			var _p72 = A2(
				_elm_community$list_extra$List_Extra$span,
				eq(_p73),
				_p71._1);
			var ys = _p72._0;
			var zs = _p72._1;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: _p73, _1: ys},
				_1: A2(_elm_community$list_extra$List_Extra$groupWhile, eq, zs)
			};
		}
	});
var _elm_community$list_extra$List_Extra$group = _elm_community$list_extra$List_Extra$groupWhile(
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		}));
var _elm_community$list_extra$List_Extra$minimumBy = F2(
	function (f, ls) {
		var minBy = F2(
			function (x, _p74) {
				var _p75 = _p74;
				var _p76 = _p75._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p76) < 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p75._0, _1: _p76};
			});
		var _p77 = ls;
		if (_p77.ctor === '::') {
			if (_p77._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p77._0);
			} else {
				var _p78 = _p77._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							minBy,
							{
								ctor: '_Tuple2',
								_0: _p78,
								_1: f(_p78)
							},
							_p77._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$maximumBy = F2(
	function (f, ls) {
		var maxBy = F2(
			function (x, _p79) {
				var _p80 = _p79;
				var _p81 = _p80._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p81) > 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p80._0, _1: _p81};
			});
		var _p82 = ls;
		if (_p82.ctor === '::') {
			if (_p82._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p82._0);
			} else {
				var _p83 = _p82._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							maxBy,
							{
								ctor: '_Tuple2',
								_0: _p83,
								_1: f(_p83)
							},
							_p82._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$uncons = function (xs) {
	var _p84 = xs;
	if (_p84.ctor === '[]') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			{ctor: '_Tuple2', _0: _p84._0, _1: _p84._1});
	}
};
var _elm_community$list_extra$List_Extra$swapAt = F3(
	function (index1, index2, l) {
		swapAt:
		while (true) {
			if (_elm_lang$core$Native_Utils.eq(index1, index2)) {
				return _elm_lang$core$Maybe$Just(l);
			} else {
				if (_elm_lang$core$Native_Utils.cmp(index1, index2) > 0) {
					var _v59 = index2,
						_v60 = index1,
						_v61 = l;
					index1 = _v59;
					index2 = _v60;
					l = _v61;
					continue swapAt;
				} else {
					if (_elm_lang$core$Native_Utils.cmp(index1, 0) < 0) {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						var _p85 = A2(_elm_community$list_extra$List_Extra$splitAt, index1, l);
						var part1 = _p85._0;
						var tail1 = _p85._1;
						var _p86 = A2(_elm_community$list_extra$List_Extra$splitAt, index2 - index1, tail1);
						var head2 = _p86._0;
						var tail2 = _p86._1;
						return A3(
							_elm_lang$core$Maybe$map2,
							F2(
								function (_p88, _p87) {
									var _p89 = _p88;
									var _p90 = _p87;
									return _elm_lang$core$List$concat(
										{
											ctor: '::',
											_0: part1,
											_1: {
												ctor: '::',
												_0: {ctor: '::', _0: _p90._0, _1: _p89._1},
												_1: {
													ctor: '::',
													_0: {ctor: '::', _0: _p89._0, _1: _p90._1},
													_1: {ctor: '[]'}
												}
											}
										});
								}),
							_elm_community$list_extra$List_Extra$uncons(head2),
							_elm_community$list_extra$List_Extra$uncons(tail2));
					}
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$iterate = F2(
	function (f, x) {
		var _p91 = f(x);
		if (_p91.ctor === 'Just') {
			return {
				ctor: '::',
				_0: x,
				_1: A2(_elm_community$list_extra$List_Extra$iterate, f, _p91._0)
			};
		} else {
			return {
				ctor: '::',
				_0: x,
				_1: {ctor: '[]'}
			};
		}
	});
var _elm_community$list_extra$List_Extra$getAt = F2(
	function (idx, xs) {
		return (_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$List$head(
			A2(_elm_lang$core$List$drop, idx, xs));
	});
var _elm_community$list_extra$List_Extra_ops = _elm_community$list_extra$List_Extra_ops || {};
_elm_community$list_extra$List_Extra_ops['!!'] = _elm_lang$core$Basics$flip(_elm_community$list_extra$List_Extra$getAt);
var _elm_community$list_extra$List_Extra$init = function () {
	var maybe = F2(
		function (d, f) {
			return function (_p92) {
				return A2(
					_elm_lang$core$Maybe$withDefault,
					d,
					A2(_elm_lang$core$Maybe$map, f, _p92));
			};
		});
	return A2(
		_elm_lang$core$List$foldr,
		function (x) {
			return function (_p93) {
				return _elm_lang$core$Maybe$Just(
					A3(
						maybe,
						{ctor: '[]'},
						F2(
							function (x, y) {
								return {ctor: '::', _0: x, _1: y};
							})(x),
						_p93));
			};
		},
		_elm_lang$core$Maybe$Nothing);
}();
var _elm_community$list_extra$List_Extra$last = _elm_community$list_extra$List_Extra$foldl1(
	_elm_lang$core$Basics$flip(_elm_lang$core$Basics$always));

var _elm_community$maybe_extra$Maybe_Extra$foldrValues = F2(
	function (item, list) {
		var _p0 = item;
		if (_p0.ctor === 'Nothing') {
			return list;
		} else {
			return {ctor: '::', _0: _p0._0, _1: list};
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$values = A2(
	_elm_lang$core$List$foldr,
	_elm_community$maybe_extra$Maybe_Extra$foldrValues,
	{ctor: '[]'});
var _elm_community$maybe_extra$Maybe_Extra$filter = F2(
	function (f, m) {
		var _p1 = A2(_elm_lang$core$Maybe$map, f, m);
		if ((_p1.ctor === 'Just') && (_p1._0 === true)) {
			return m;
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$traverseArray = function (f) {
	var step = F2(
		function (e, acc) {
			var _p2 = f(e);
			if (_p2.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return A2(
					_elm_lang$core$Maybe$map,
					_elm_lang$core$Array$push(_p2._0),
					acc);
			}
		});
	return A2(
		_elm_lang$core$Array$foldl,
		step,
		_elm_lang$core$Maybe$Just(_elm_lang$core$Array$empty));
};
var _elm_community$maybe_extra$Maybe_Extra$combineArray = _elm_community$maybe_extra$Maybe_Extra$traverseArray(_elm_lang$core$Basics$identity);
var _elm_community$maybe_extra$Maybe_Extra$traverse = function (f) {
	var step = F2(
		function (e, acc) {
			var _p3 = f(e);
			if (_p3.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				return A2(
					_elm_lang$core$Maybe$map,
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						})(_p3._0),
					acc);
			}
		});
	return A2(
		_elm_lang$core$List$foldr,
		step,
		_elm_lang$core$Maybe$Just(
			{ctor: '[]'}));
};
var _elm_community$maybe_extra$Maybe_Extra$combine = _elm_community$maybe_extra$Maybe_Extra$traverse(_elm_lang$core$Basics$identity);
var _elm_community$maybe_extra$Maybe_Extra$toArray = function (m) {
	var _p4 = m;
	if (_p4.ctor === 'Nothing') {
		return _elm_lang$core$Array$empty;
	} else {
		return A2(_elm_lang$core$Array$repeat, 1, _p4._0);
	}
};
var _elm_community$maybe_extra$Maybe_Extra$toList = function (m) {
	var _p5 = m;
	if (_p5.ctor === 'Nothing') {
		return {ctor: '[]'};
	} else {
		return {
			ctor: '::',
			_0: _p5._0,
			_1: {ctor: '[]'}
		};
	}
};
var _elm_community$maybe_extra$Maybe_Extra$orElse = F2(
	function (ma, mb) {
		var _p6 = mb;
		if (_p6.ctor === 'Nothing') {
			return ma;
		} else {
			return mb;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$orElseLazy = F2(
	function (fma, mb) {
		var _p7 = mb;
		if (_p7.ctor === 'Nothing') {
			return fma(
				{ctor: '_Tuple0'});
		} else {
			return mb;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$orLazy = F2(
	function (ma, fmb) {
		var _p8 = ma;
		if (_p8.ctor === 'Nothing') {
			return fmb(
				{ctor: '_Tuple0'});
		} else {
			return ma;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$or = F2(
	function (ma, mb) {
		var _p9 = ma;
		if (_p9.ctor === 'Nothing') {
			return mb;
		} else {
			return ma;
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$prev = _elm_lang$core$Maybe$map2(_elm_lang$core$Basics$always);
var _elm_community$maybe_extra$Maybe_Extra$next = _elm_lang$core$Maybe$map2(
	_elm_lang$core$Basics$flip(_elm_lang$core$Basics$always));
var _elm_community$maybe_extra$Maybe_Extra$andMap = _elm_lang$core$Maybe$map2(
	F2(
		function (x, y) {
			return y(x);
		}));
var _elm_community$maybe_extra$Maybe_Extra$unpack = F3(
	function (d, f, m) {
		var _p10 = m;
		if (_p10.ctor === 'Nothing') {
			return d(
				{ctor: '_Tuple0'});
		} else {
			return f(_p10._0);
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$unwrap = F3(
	function (d, f, m) {
		var _p11 = m;
		if (_p11.ctor === 'Nothing') {
			return d;
		} else {
			return f(_p11._0);
		}
	});
var _elm_community$maybe_extra$Maybe_Extra$isJust = function (m) {
	var _p12 = m;
	if (_p12.ctor === 'Nothing') {
		return false;
	} else {
		return true;
	}
};
var _elm_community$maybe_extra$Maybe_Extra$isNothing = function (m) {
	var _p13 = m;
	if (_p13.ctor === 'Nothing') {
		return true;
	} else {
		return false;
	}
};
var _elm_community$maybe_extra$Maybe_Extra$join = function (mx) {
	var _p14 = mx;
	if (_p14.ctor === 'Just') {
		return _p14._0;
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_community$maybe_extra$Maybe_Extra_ops = _elm_community$maybe_extra$Maybe_Extra_ops || {};
_elm_community$maybe_extra$Maybe_Extra_ops['?'] = F2(
	function (mx, x) {
		return A2(_elm_lang$core$Maybe$withDefault, x, mx);
	});

var _elm_lang$core$Color$fmod = F2(
	function (f, n) {
		var integer = _elm_lang$core$Basics$floor(f);
		return (_elm_lang$core$Basics$toFloat(
			A2(_elm_lang$core$Basics_ops['%'], integer, n)) + f) - _elm_lang$core$Basics$toFloat(integer);
	});
var _elm_lang$core$Color$rgbToHsl = F3(
	function (red, green, blue) {
		var b = _elm_lang$core$Basics$toFloat(blue) / 255;
		var g = _elm_lang$core$Basics$toFloat(green) / 255;
		var r = _elm_lang$core$Basics$toFloat(red) / 255;
		var cMax = A2(
			_elm_lang$core$Basics$max,
			A2(_elm_lang$core$Basics$max, r, g),
			b);
		var cMin = A2(
			_elm_lang$core$Basics$min,
			A2(_elm_lang$core$Basics$min, r, g),
			b);
		var c = cMax - cMin;
		var lightness = (cMax + cMin) / 2;
		var saturation = _elm_lang$core$Native_Utils.eq(lightness, 0) ? 0 : (c / (1 - _elm_lang$core$Basics$abs((2 * lightness) - 1)));
		var hue = _elm_lang$core$Basics$degrees(60) * (_elm_lang$core$Native_Utils.eq(cMax, r) ? A2(_elm_lang$core$Color$fmod, (g - b) / c, 6) : (_elm_lang$core$Native_Utils.eq(cMax, g) ? (((b - r) / c) + 2) : (((r - g) / c) + 4)));
		return {ctor: '_Tuple3', _0: hue, _1: saturation, _2: lightness};
	});
var _elm_lang$core$Color$hslToRgb = F3(
	function (hue, saturation, lightness) {
		var normHue = hue / _elm_lang$core$Basics$degrees(60);
		var chroma = (1 - _elm_lang$core$Basics$abs((2 * lightness) - 1)) * saturation;
		var x = chroma * (1 - _elm_lang$core$Basics$abs(
			A2(_elm_lang$core$Color$fmod, normHue, 2) - 1));
		var _p0 = (_elm_lang$core$Native_Utils.cmp(normHue, 0) < 0) ? {ctor: '_Tuple3', _0: 0, _1: 0, _2: 0} : ((_elm_lang$core$Native_Utils.cmp(normHue, 1) < 0) ? {ctor: '_Tuple3', _0: chroma, _1: x, _2: 0} : ((_elm_lang$core$Native_Utils.cmp(normHue, 2) < 0) ? {ctor: '_Tuple3', _0: x, _1: chroma, _2: 0} : ((_elm_lang$core$Native_Utils.cmp(normHue, 3) < 0) ? {ctor: '_Tuple3', _0: 0, _1: chroma, _2: x} : ((_elm_lang$core$Native_Utils.cmp(normHue, 4) < 0) ? {ctor: '_Tuple3', _0: 0, _1: x, _2: chroma} : ((_elm_lang$core$Native_Utils.cmp(normHue, 5) < 0) ? {ctor: '_Tuple3', _0: x, _1: 0, _2: chroma} : ((_elm_lang$core$Native_Utils.cmp(normHue, 6) < 0) ? {ctor: '_Tuple3', _0: chroma, _1: 0, _2: x} : {ctor: '_Tuple3', _0: 0, _1: 0, _2: 0}))))));
		var r = _p0._0;
		var g = _p0._1;
		var b = _p0._2;
		var m = lightness - (chroma / 2);
		return {ctor: '_Tuple3', _0: r + m, _1: g + m, _2: b + m};
	});
var _elm_lang$core$Color$toRgb = function (color) {
	var _p1 = color;
	if (_p1.ctor === 'RGBA') {
		return {red: _p1._0, green: _p1._1, blue: _p1._2, alpha: _p1._3};
	} else {
		var _p2 = A3(_elm_lang$core$Color$hslToRgb, _p1._0, _p1._1, _p1._2);
		var r = _p2._0;
		var g = _p2._1;
		var b = _p2._2;
		return {
			red: _elm_lang$core$Basics$round(255 * r),
			green: _elm_lang$core$Basics$round(255 * g),
			blue: _elm_lang$core$Basics$round(255 * b),
			alpha: _p1._3
		};
	}
};
var _elm_lang$core$Color$toHsl = function (color) {
	var _p3 = color;
	if (_p3.ctor === 'HSLA') {
		return {hue: _p3._0, saturation: _p3._1, lightness: _p3._2, alpha: _p3._3};
	} else {
		var _p4 = A3(_elm_lang$core$Color$rgbToHsl, _p3._0, _p3._1, _p3._2);
		var h = _p4._0;
		var s = _p4._1;
		var l = _p4._2;
		return {hue: h, saturation: s, lightness: l, alpha: _p3._3};
	}
};
var _elm_lang$core$Color$HSLA = F4(
	function (a, b, c, d) {
		return {ctor: 'HSLA', _0: a, _1: b, _2: c, _3: d};
	});
var _elm_lang$core$Color$hsla = F4(
	function (hue, saturation, lightness, alpha) {
		return A4(
			_elm_lang$core$Color$HSLA,
			hue - _elm_lang$core$Basics$turns(
				_elm_lang$core$Basics$toFloat(
					_elm_lang$core$Basics$floor(hue / (2 * _elm_lang$core$Basics$pi)))),
			saturation,
			lightness,
			alpha);
	});
var _elm_lang$core$Color$hsl = F3(
	function (hue, saturation, lightness) {
		return A4(_elm_lang$core$Color$hsla, hue, saturation, lightness, 1);
	});
var _elm_lang$core$Color$complement = function (color) {
	var _p5 = color;
	if (_p5.ctor === 'HSLA') {
		return A4(
			_elm_lang$core$Color$hsla,
			_p5._0 + _elm_lang$core$Basics$degrees(180),
			_p5._1,
			_p5._2,
			_p5._3);
	} else {
		var _p6 = A3(_elm_lang$core$Color$rgbToHsl, _p5._0, _p5._1, _p5._2);
		var h = _p6._0;
		var s = _p6._1;
		var l = _p6._2;
		return A4(
			_elm_lang$core$Color$hsla,
			h + _elm_lang$core$Basics$degrees(180),
			s,
			l,
			_p5._3);
	}
};
var _elm_lang$core$Color$grayscale = function (p) {
	return A4(_elm_lang$core$Color$HSLA, 0, 0, 1 - p, 1);
};
var _elm_lang$core$Color$greyscale = function (p) {
	return A4(_elm_lang$core$Color$HSLA, 0, 0, 1 - p, 1);
};
var _elm_lang$core$Color$RGBA = F4(
	function (a, b, c, d) {
		return {ctor: 'RGBA', _0: a, _1: b, _2: c, _3: d};
	});
var _elm_lang$core$Color$rgba = _elm_lang$core$Color$RGBA;
var _elm_lang$core$Color$rgb = F3(
	function (r, g, b) {
		return A4(_elm_lang$core$Color$RGBA, r, g, b, 1);
	});
var _elm_lang$core$Color$lightRed = A4(_elm_lang$core$Color$RGBA, 239, 41, 41, 1);
var _elm_lang$core$Color$red = A4(_elm_lang$core$Color$RGBA, 204, 0, 0, 1);
var _elm_lang$core$Color$darkRed = A4(_elm_lang$core$Color$RGBA, 164, 0, 0, 1);
var _elm_lang$core$Color$lightOrange = A4(_elm_lang$core$Color$RGBA, 252, 175, 62, 1);
var _elm_lang$core$Color$orange = A4(_elm_lang$core$Color$RGBA, 245, 121, 0, 1);
var _elm_lang$core$Color$darkOrange = A4(_elm_lang$core$Color$RGBA, 206, 92, 0, 1);
var _elm_lang$core$Color$lightYellow = A4(_elm_lang$core$Color$RGBA, 255, 233, 79, 1);
var _elm_lang$core$Color$yellow = A4(_elm_lang$core$Color$RGBA, 237, 212, 0, 1);
var _elm_lang$core$Color$darkYellow = A4(_elm_lang$core$Color$RGBA, 196, 160, 0, 1);
var _elm_lang$core$Color$lightGreen = A4(_elm_lang$core$Color$RGBA, 138, 226, 52, 1);
var _elm_lang$core$Color$green = A4(_elm_lang$core$Color$RGBA, 115, 210, 22, 1);
var _elm_lang$core$Color$darkGreen = A4(_elm_lang$core$Color$RGBA, 78, 154, 6, 1);
var _elm_lang$core$Color$lightBlue = A4(_elm_lang$core$Color$RGBA, 114, 159, 207, 1);
var _elm_lang$core$Color$blue = A4(_elm_lang$core$Color$RGBA, 52, 101, 164, 1);
var _elm_lang$core$Color$darkBlue = A4(_elm_lang$core$Color$RGBA, 32, 74, 135, 1);
var _elm_lang$core$Color$lightPurple = A4(_elm_lang$core$Color$RGBA, 173, 127, 168, 1);
var _elm_lang$core$Color$purple = A4(_elm_lang$core$Color$RGBA, 117, 80, 123, 1);
var _elm_lang$core$Color$darkPurple = A4(_elm_lang$core$Color$RGBA, 92, 53, 102, 1);
var _elm_lang$core$Color$lightBrown = A4(_elm_lang$core$Color$RGBA, 233, 185, 110, 1);
var _elm_lang$core$Color$brown = A4(_elm_lang$core$Color$RGBA, 193, 125, 17, 1);
var _elm_lang$core$Color$darkBrown = A4(_elm_lang$core$Color$RGBA, 143, 89, 2, 1);
var _elm_lang$core$Color$black = A4(_elm_lang$core$Color$RGBA, 0, 0, 0, 1);
var _elm_lang$core$Color$white = A4(_elm_lang$core$Color$RGBA, 255, 255, 255, 1);
var _elm_lang$core$Color$lightGrey = A4(_elm_lang$core$Color$RGBA, 238, 238, 236, 1);
var _elm_lang$core$Color$grey = A4(_elm_lang$core$Color$RGBA, 211, 215, 207, 1);
var _elm_lang$core$Color$darkGrey = A4(_elm_lang$core$Color$RGBA, 186, 189, 182, 1);
var _elm_lang$core$Color$lightGray = A4(_elm_lang$core$Color$RGBA, 238, 238, 236, 1);
var _elm_lang$core$Color$gray = A4(_elm_lang$core$Color$RGBA, 211, 215, 207, 1);
var _elm_lang$core$Color$darkGray = A4(_elm_lang$core$Color$RGBA, 186, 189, 182, 1);
var _elm_lang$core$Color$lightCharcoal = A4(_elm_lang$core$Color$RGBA, 136, 138, 133, 1);
var _elm_lang$core$Color$charcoal = A4(_elm_lang$core$Color$RGBA, 85, 87, 83, 1);
var _elm_lang$core$Color$darkCharcoal = A4(_elm_lang$core$Color$RGBA, 46, 52, 54, 1);
var _elm_lang$core$Color$Radial = F5(
	function (a, b, c, d, e) {
		return {ctor: 'Radial', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Color$radial = _elm_lang$core$Color$Radial;
var _elm_lang$core$Color$Linear = F3(
	function (a, b, c) {
		return {ctor: 'Linear', _0: a, _1: b, _2: c};
	});
var _elm_lang$core$Color$linear = _elm_lang$core$Color$Linear;

var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
var _elm_lang$core$Task$spawnCmd = F2(
	function (router, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Scheduler.spawn(
			A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Platform$sendToApp(router),
				_p1._0));
	});
var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
var _elm_lang$core$Task$mapError = F2(
	function (convert, task) {
		return A2(
			_elm_lang$core$Task$onError,
			function (_p2) {
				return _elm_lang$core$Task$fail(
					convert(_p2));
			},
			task);
	});
var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return _elm_lang$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var _elm_lang$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return _elm_lang$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map3 = F4(
	function (func, taskA, taskB, taskC) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return _elm_lang$core$Task$succeed(
									A3(func, a, b, c));
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map4 = F5(
	function (func, taskA, taskB, taskC, taskD) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return _elm_lang$core$Task$succeed(
											A4(func, a, b, c, d));
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map5 = F6(
	function (func, taskA, taskB, taskC, taskD, taskE) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Task$andThen,
											function (e) {
												return _elm_lang$core$Task$succeed(
													A5(func, a, b, c, d, e));
											},
											taskE);
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$sequence = function (tasks) {
	var _p3 = tasks;
	if (_p3.ctor === '[]') {
		return _elm_lang$core$Task$succeed(
			{ctor: '[]'});
	} else {
		return A3(
			_elm_lang$core$Task$map2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p3._0,
			_elm_lang$core$Task$sequence(_p3._1));
	}
};
var _elm_lang$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			_elm_lang$core$Task$map,
			function (_p4) {
				return {ctor: '_Tuple0'};
			},
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Task$spawnCmd(router),
					commands)));
	});
var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
	{ctor: '_Tuple0'});
var _elm_lang$core$Task$onSelfMsg = F3(
	function (_p7, _p6, _p5) {
		return _elm_lang$core$Task$succeed(
			{ctor: '_Tuple0'});
	});
var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
var _elm_lang$core$Task$Perform = function (a) {
	return {ctor: 'Perform', _0: a};
};
var _elm_lang$core$Task$perform = F2(
	function (toMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(_elm_lang$core$Task$map, toMessage, task)));
	});
var _elm_lang$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(
					_elm_lang$core$Task$onError,
					function (_p8) {
						return _elm_lang$core$Task$succeed(
							resultToMessage(
								_elm_lang$core$Result$Err(_p8)));
					},
					A2(
						_elm_lang$core$Task$andThen,
						function (_p9) {
							return _elm_lang$core$Task$succeed(
								resultToMessage(
									_elm_lang$core$Result$Ok(_p9)));
						},
						task))));
	});
var _elm_lang$core$Task$cmdMap = F2(
	function (tagger, _p10) {
		var _p11 = _p10;
		return _elm_lang$core$Task$Perform(
			A2(_elm_lang$core$Task$map, tagger, _p11._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};

//import Native.Scheduler //

var _elm_lang$core$Native_Time = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
});

function setInterval_(interval, task)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = setInterval(function() {
			_elm_lang$core$Native_Scheduler.rawSpawn(task);
		}, interval);

		return function() { clearInterval(id); };
	});
}

return {
	now: now,
	setInterval_: F2(setInterval_)
};

}();
var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
var _elm_lang$core$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		var _p0 = intervals;
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Task$succeed(processes);
		} else {
			var _p1 = _p0._0;
			var spawnRest = function (id) {
				return A3(
					_elm_lang$core$Time$spawnHelp,
					router,
					_p0._1,
					A3(_elm_lang$core$Dict$insert, _p1, id, processes));
			};
			var spawnTimer = _elm_lang$core$Native_Scheduler.spawn(
				A2(
					_elm_lang$core$Time$setInterval,
					_p1,
					A2(_elm_lang$core$Platform$sendToSelf, router, _p1)));
			return A2(_elm_lang$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var _elm_lang$core$Time$addMySub = F2(
	function (_p2, state) {
		var _p3 = _p2;
		var _p6 = _p3._1;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
		if (_p4.ctor === 'Nothing') {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{
					ctor: '::',
					_0: _p6,
					_1: {ctor: '[]'}
				},
				state);
		} else {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{ctor: '::', _0: _p6, _1: _p4._0},
				state);
		}
	});
var _elm_lang$core$Time$inMilliseconds = function (t) {
	return t;
};
var _elm_lang$core$Time$millisecond = 1;
var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
var _elm_lang$core$Time$inHours = function (t) {
	return t / _elm_lang$core$Time$hour;
};
var _elm_lang$core$Time$inMinutes = function (t) {
	return t / _elm_lang$core$Time$minute;
};
var _elm_lang$core$Time$inSeconds = function (t) {
	return t / _elm_lang$core$Time$second;
};
var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
var _elm_lang$core$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
		if (_p7.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var tellTaggers = function (time) {
				return _elm_lang$core$Task$sequence(
					A2(
						_elm_lang$core$List$map,
						function (tagger) {
							return A2(
								_elm_lang$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						_p7._0));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p8) {
					return _elm_lang$core$Task$succeed(state);
				},
				A2(_elm_lang$core$Task$andThen, tellTaggers, _elm_lang$core$Time$now));
		}
	});
var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
var _elm_lang$core$Time$State = F2(
	function (a, b) {
		return {taggers: a, processes: b};
	});
var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
	A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
var _elm_lang$core$Time$onEffects = F3(
	function (router, subs, _p9) {
		var _p10 = _p9;
		var rightStep = F3(
			function (_p12, id, _p11) {
				var _p13 = _p11;
				return {
					ctor: '_Tuple3',
					_0: _p13._0,
					_1: _p13._1,
					_2: A2(
						_elm_lang$core$Task$andThen,
						function (_p14) {
							return _p13._2;
						},
						_elm_lang$core$Native_Scheduler.kill(id))
				};
			});
		var bothStep = F4(
			function (interval, taggers, id, _p15) {
				var _p16 = _p15;
				return {
					ctor: '_Tuple3',
					_0: _p16._0,
					_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
					_2: _p16._2
				};
			});
		var leftStep = F3(
			function (interval, taggers, _p17) {
				var _p18 = _p17;
				return {
					ctor: '_Tuple3',
					_0: {ctor: '::', _0: interval, _1: _p18._0},
					_1: _p18._1,
					_2: _p18._2
				};
			});
		var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
		var _p19 = A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			_p10.processes,
			{
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _elm_lang$core$Dict$empty,
				_2: _elm_lang$core$Task$succeed(
					{ctor: '_Tuple0'})
			});
		var spawnList = _p19._0;
		var existingDict = _p19._1;
		var killTask = _p19._2;
		return A2(
			_elm_lang$core$Task$andThen,
			function (newProcesses) {
				return _elm_lang$core$Task$succeed(
					A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
			},
			A2(
				_elm_lang$core$Task$andThen,
				function (_p20) {
					return A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var _elm_lang$core$Time$Every = F2(
	function (a, b) {
		return {ctor: 'Every', _0: a, _1: b};
	});
var _elm_lang$core$Time$every = F2(
	function (interval, tagger) {
		return _elm_lang$core$Time$subscription(
			A2(_elm_lang$core$Time$Every, interval, tagger));
	});
var _elm_lang$core$Time$subMap = F2(
	function (f, _p21) {
		var _p22 = _p21;
		return A2(
			_elm_lang$core$Time$Every,
			_p22._0,
			function (_p23) {
				return f(
					_p22._1(_p23));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};

//import Maybe, Native.List //

var _elm_lang$core$Native_Regex = function() {

function escape(str)
{
	return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
}
function caseInsensitive(re)
{
	return new RegExp(re.source, 'gi');
}
function regex(raw)
{
	return new RegExp(raw, 'g');
}

function contains(re, string)
{
	return string.match(re) !== null;
}

function find(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex === re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		out.push({
			match: result[0],
			submatches: _elm_lang$core$Native_List.fromArray(subs),
			index: result.index,
			number: number
		});
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

function replace(n, re, replacer, string)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		return replacer({
			match: match,
			submatches: _elm_lang$core$Native_List.fromArray(submatches),
			index: arguments[arguments.length - 2],
			number: count
		});
	}
	return string.replace(re, jsReplacer);
}

function split(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	if (n === Infinity)
	{
		return _elm_lang$core$Native_List.fromArray(str.split(re));
	}
	var string = str;
	var result;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		if (!(result = re.exec(string))) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

return {
	regex: regex,
	caseInsensitive: caseInsensitive,
	escape: escape,

	contains: F2(contains),
	find: F3(find),
	replace: F4(replace),
	split: F3(split)
};

}();

var _elm_lang$core$Process$kill = _elm_lang$core$Native_Scheduler.kill;
var _elm_lang$core$Process$sleep = _elm_lang$core$Native_Scheduler.sleep;
var _elm_lang$core$Process$spawn = _elm_lang$core$Native_Scheduler.spawn;

var _elm_lang$core$Regex$split = _elm_lang$core$Native_Regex.split;
var _elm_lang$core$Regex$replace = _elm_lang$core$Native_Regex.replace;
var _elm_lang$core$Regex$find = _elm_lang$core$Native_Regex.find;
var _elm_lang$core$Regex$contains = _elm_lang$core$Native_Regex.contains;
var _elm_lang$core$Regex$caseInsensitive = _elm_lang$core$Native_Regex.caseInsensitive;
var _elm_lang$core$Regex$regex = _elm_lang$core$Native_Regex.regex;
var _elm_lang$core$Regex$escape = _elm_lang$core$Native_Regex.escape;
var _elm_lang$core$Regex$Match = F4(
	function (a, b, c, d) {
		return {match: a, submatches: b, index: c, number: d};
	});
var _elm_lang$core$Regex$Regex = {ctor: 'Regex'};
var _elm_lang$core$Regex$AtMost = function (a) {
	return {ctor: 'AtMost', _0: a};
};
var _elm_lang$core$Regex$All = {ctor: 'All'};

var _elm_lang$dom$Native_Dom = function() {

var fakeNode = {
	addEventListener: function() {},
	removeEventListener: function() {}
};

var onDocument = on(typeof document !== 'undefined' ? document : fakeNode);
var onWindow = on(typeof window !== 'undefined' ? window : fakeNode);

function on(node)
{
	return function(eventName, decoder, toTask)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {

			function performTask(event)
			{
				var result = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, event);
				if (result.ctor === 'Ok')
				{
					_elm_lang$core$Native_Scheduler.rawSpawn(toTask(result._0));
				}
			}

			node.addEventListener(eventName, performTask);

			return function()
			{
				node.removeEventListener(eventName, performTask);
			};
		});
	};
}

var rAF = typeof requestAnimationFrame !== 'undefined'
	? requestAnimationFrame
	: function(callback) { callback(); };

function withNode(id, doStuff)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		rAF(function()
		{
			var node = document.getElementById(id);
			if (node === null)
			{
				callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NotFound', _0: id }));
				return;
			}
			callback(_elm_lang$core$Native_Scheduler.succeed(doStuff(node)));
		});
	});
}


// FOCUS

function focus(id)
{
	return withNode(id, function(node) {
		node.focus();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function blur(id)
{
	return withNode(id, function(node) {
		node.blur();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SCROLLING

function getScrollTop(id)
{
	return withNode(id, function(node) {
		return node.scrollTop;
	});
}

function setScrollTop(id, desiredScrollTop)
{
	return withNode(id, function(node) {
		node.scrollTop = desiredScrollTop;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toBottom(id)
{
	return withNode(id, function(node) {
		node.scrollTop = node.scrollHeight;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function getScrollLeft(id)
{
	return withNode(id, function(node) {
		return node.scrollLeft;
	});
}

function setScrollLeft(id, desiredScrollLeft)
{
	return withNode(id, function(node) {
		node.scrollLeft = desiredScrollLeft;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toRight(id)
{
	return withNode(id, function(node) {
		node.scrollLeft = node.scrollWidth;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SIZE

function width(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollWidth;
			case 'VisibleContent':
				return node.clientWidth;
			case 'VisibleContentWithBorders':
				return node.offsetWidth;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.right - rect.left;
		}
	});
}

function height(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollHeight;
			case 'VisibleContent':
				return node.clientHeight;
			case 'VisibleContentWithBorders':
				return node.offsetHeight;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.bottom - rect.top;
		}
	});
}

return {
	onDocument: F3(onDocument),
	onWindow: F3(onWindow),

	focus: focus,
	blur: blur,

	getScrollTop: getScrollTop,
	setScrollTop: F2(setScrollTop),
	getScrollLeft: getScrollLeft,
	setScrollLeft: F2(setScrollLeft),
	toBottom: toBottom,
	toRight: toRight,

	height: F2(height),
	width: F2(width)
};

}();

var _elm_lang$dom$Dom_LowLevel$onWindow = _elm_lang$dom$Native_Dom.onWindow;
var _elm_lang$dom$Dom_LowLevel$onDocument = _elm_lang$dom$Native_Dom.onDocument;

var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$html$Html_Events$targetChecked = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'checked',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$bool);
var _elm_lang$html$Html_Events$targetValue = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'value',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$string);
var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$html$Html_Events$onFocus = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onBlur = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
	_elm_lang$html$Html_Events$defaultOptions,
	{preventDefault: true});
var _elm_lang$html$Html_Events$onSubmit = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'submit',
		_elm_lang$html$Html_Events$onSubmitOptions,
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onCheck = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'change',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
};
var _elm_lang$html$Html_Events$onInput = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
};
var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});

var _elm_lang$keyboard$Keyboard$onSelfMsg = F3(
	function (router, _p0, state) {
		var _p1 = _p0;
		var _p2 = A2(_elm_lang$core$Dict$get, _p1.category, state);
		if (_p2.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p1.keyCode));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p3) {
					return _elm_lang$core$Task$succeed(state);
				},
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p2._0.taggers)));
		}
	});
var _elm_lang$keyboard$Keyboard_ops = _elm_lang$keyboard$Keyboard_ops || {};
_elm_lang$keyboard$Keyboard_ops['&>'] = F2(
	function (task1, task2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p4) {
				return task2;
			},
			task1);
	});
var _elm_lang$keyboard$Keyboard$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$keyboard$Keyboard$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p5 = maybeValues;
		if (_p5.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p5._0});
		}
	});
var _elm_lang$keyboard$Keyboard$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p6 = subs;
			if (_p6.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p6._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p6._0._0,
					_elm_lang$keyboard$Keyboard$categorizeHelpHelp(_p6._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$keyboard$Keyboard$categorize = function (subs) {
	return A2(_elm_lang$keyboard$Keyboard$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$keyboard$Keyboard$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$keyboard$Keyboard$subscription = _elm_lang$core$Native_Platform.leaf('Keyboard');
var _elm_lang$keyboard$Keyboard$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$keyboard$Keyboard$Msg = F2(
	function (a, b) {
		return {category: a, keyCode: b};
	});
var _elm_lang$keyboard$Keyboard$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(
								A3(
									_elm_lang$dom$Dom_LowLevel$onDocument,
									category,
									_elm_lang$keyboard$Keyboard$keyCode,
									function (_p7) {
										return A2(
											_elm_lang$core$Platform$sendToSelf,
											router,
											A2(_elm_lang$keyboard$Keyboard$Msg, category, _p7));
									})));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p8, taggers, task) {
				var _p9 = _p8;
				return A2(
					_elm_lang$core$Task$map,
					A2(
						_elm_lang$core$Dict$insert,
						category,
						A2(_elm_lang$keyboard$Keyboard$Watcher, taggers, _p9.pid)),
					task);
			});
		var leftStep = F3(
			function (category, _p10, task) {
				var _p11 = _p10;
				return A2(
					_elm_lang$keyboard$Keyboard_ops['&>'],
					_elm_lang$core$Process$kill(_p11.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$keyboard$Keyboard$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$keyboard$Keyboard$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$keyboard$Keyboard$presses = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keypress', tagger));
};
var _elm_lang$keyboard$Keyboard$downs = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keydown', tagger));
};
var _elm_lang$keyboard$Keyboard$ups = function (tagger) {
	return _elm_lang$keyboard$Keyboard$subscription(
		A2(_elm_lang$keyboard$Keyboard$MySub, 'keyup', tagger));
};
var _elm_lang$keyboard$Keyboard$subMap = F2(
	function (func, _p12) {
		var _p13 = _p12;
		return A2(
			_elm_lang$keyboard$Keyboard$MySub,
			_p13._0,
			function (_p14) {
				return func(
					_p13._1(_p14));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Keyboard'] = {pkg: 'elm-lang/keyboard', init: _elm_lang$keyboard$Keyboard$init, onEffects: _elm_lang$keyboard$Keyboard$onEffects, onSelfMsg: _elm_lang$keyboard$Keyboard$onSelfMsg, tag: 'sub', subMap: _elm_lang$keyboard$Keyboard$subMap};

var _elm_lang$mouse$Mouse_ops = _elm_lang$mouse$Mouse_ops || {};
_elm_lang$mouse$Mouse_ops['&>'] = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return t2;
			},
			t1);
	});
var _elm_lang$mouse$Mouse$onSelfMsg = F3(
	function (router, _p1, state) {
		var _p2 = _p1;
		var _p3 = A2(_elm_lang$core$Dict$get, _p2.category, state);
		if (_p3.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p2.position));
			};
			return A2(
				_elm_lang$mouse$Mouse_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p3._0.taggers)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$mouse$Mouse$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$mouse$Mouse$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p4 = maybeValues;
		if (_p4.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p4._0});
		}
	});
var _elm_lang$mouse$Mouse$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p5 = subs;
			if (_p5.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p5._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p5._0._0,
					_elm_lang$mouse$Mouse$categorizeHelpHelp(_p5._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$mouse$Mouse$categorize = function (subs) {
	return A2(_elm_lang$mouse$Mouse$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$mouse$Mouse$subscription = _elm_lang$core$Native_Platform.leaf('Mouse');
var _elm_lang$mouse$Mouse$Position = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _elm_lang$mouse$Mouse$position = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$mouse$Mouse$Position,
	A2(_elm_lang$core$Json_Decode$field, 'pageX', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'pageY', _elm_lang$core$Json_Decode$int));
var _elm_lang$mouse$Mouse$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$mouse$Mouse$Msg = F2(
	function (a, b) {
		return {category: a, position: b};
	});
var _elm_lang$mouse$Mouse$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				var tracker = A3(
					_elm_lang$dom$Dom_LowLevel$onDocument,
					category,
					_elm_lang$mouse$Mouse$position,
					function (_p6) {
						return A2(
							_elm_lang$core$Platform$sendToSelf,
							router,
							A2(_elm_lang$mouse$Mouse$Msg, category, _p6));
					});
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$mouse$Mouse$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(tracker));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p7, taggers, task) {
				var _p8 = _p7;
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return _elm_lang$core$Task$succeed(
							A3(
								_elm_lang$core$Dict$insert,
								category,
								A2(_elm_lang$mouse$Mouse$Watcher, taggers, _p8.pid),
								state));
					},
					task);
			});
		var leftStep = F3(
			function (category, _p9, task) {
				var _p10 = _p9;
				return A2(
					_elm_lang$mouse$Mouse_ops['&>'],
					_elm_lang$core$Process$kill(_p10.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$mouse$Mouse$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$mouse$Mouse$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$mouse$Mouse$clicks = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'click', tagger));
};
var _elm_lang$mouse$Mouse$moves = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousemove', tagger));
};
var _elm_lang$mouse$Mouse$downs = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousedown', tagger));
};
var _elm_lang$mouse$Mouse$ups = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mouseup', tagger));
};
var _elm_lang$mouse$Mouse$subMap = F2(
	function (func, _p11) {
		var _p12 = _p11;
		return A2(
			_elm_lang$mouse$Mouse$MySub,
			_p12._0,
			function (_p13) {
				return func(
					_p12._1(_p13));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Mouse'] = {pkg: 'elm-lang/mouse', init: _elm_lang$mouse$Mouse$init, onEffects: _elm_lang$mouse$Mouse$onEffects, onSelfMsg: _elm_lang$mouse$Mouse$onSelfMsg, tag: 'sub', subMap: _elm_lang$mouse$Mouse$subMap};

var _elm_lang$svg$Svg$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$svg$Svg$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$svg$Svg$svgNamespace = A2(
	_elm_lang$virtual_dom$VirtualDom$property,
	'namespace',
	_elm_lang$core$Json_Encode$string('http://www.w3.org/2000/svg'));
var _elm_lang$svg$Svg$node = F3(
	function (name, attributes, children) {
		return A3(
			_elm_lang$virtual_dom$VirtualDom$node,
			name,
			{ctor: '::', _0: _elm_lang$svg$Svg$svgNamespace, _1: attributes},
			children);
	});
var _elm_lang$svg$Svg$svg = _elm_lang$svg$Svg$node('svg');
var _elm_lang$svg$Svg$foreignObject = _elm_lang$svg$Svg$node('foreignObject');
var _elm_lang$svg$Svg$animate = _elm_lang$svg$Svg$node('animate');
var _elm_lang$svg$Svg$animateColor = _elm_lang$svg$Svg$node('animateColor');
var _elm_lang$svg$Svg$animateMotion = _elm_lang$svg$Svg$node('animateMotion');
var _elm_lang$svg$Svg$animateTransform = _elm_lang$svg$Svg$node('animateTransform');
var _elm_lang$svg$Svg$mpath = _elm_lang$svg$Svg$node('mpath');
var _elm_lang$svg$Svg$set = _elm_lang$svg$Svg$node('set');
var _elm_lang$svg$Svg$a = _elm_lang$svg$Svg$node('a');
var _elm_lang$svg$Svg$defs = _elm_lang$svg$Svg$node('defs');
var _elm_lang$svg$Svg$g = _elm_lang$svg$Svg$node('g');
var _elm_lang$svg$Svg$marker = _elm_lang$svg$Svg$node('marker');
var _elm_lang$svg$Svg$mask = _elm_lang$svg$Svg$node('mask');
var _elm_lang$svg$Svg$pattern = _elm_lang$svg$Svg$node('pattern');
var _elm_lang$svg$Svg$switch = _elm_lang$svg$Svg$node('switch');
var _elm_lang$svg$Svg$symbol = _elm_lang$svg$Svg$node('symbol');
var _elm_lang$svg$Svg$desc = _elm_lang$svg$Svg$node('desc');
var _elm_lang$svg$Svg$metadata = _elm_lang$svg$Svg$node('metadata');
var _elm_lang$svg$Svg$title = _elm_lang$svg$Svg$node('title');
var _elm_lang$svg$Svg$feBlend = _elm_lang$svg$Svg$node('feBlend');
var _elm_lang$svg$Svg$feColorMatrix = _elm_lang$svg$Svg$node('feColorMatrix');
var _elm_lang$svg$Svg$feComponentTransfer = _elm_lang$svg$Svg$node('feComponentTransfer');
var _elm_lang$svg$Svg$feComposite = _elm_lang$svg$Svg$node('feComposite');
var _elm_lang$svg$Svg$feConvolveMatrix = _elm_lang$svg$Svg$node('feConvolveMatrix');
var _elm_lang$svg$Svg$feDiffuseLighting = _elm_lang$svg$Svg$node('feDiffuseLighting');
var _elm_lang$svg$Svg$feDisplacementMap = _elm_lang$svg$Svg$node('feDisplacementMap');
var _elm_lang$svg$Svg$feFlood = _elm_lang$svg$Svg$node('feFlood');
var _elm_lang$svg$Svg$feFuncA = _elm_lang$svg$Svg$node('feFuncA');
var _elm_lang$svg$Svg$feFuncB = _elm_lang$svg$Svg$node('feFuncB');
var _elm_lang$svg$Svg$feFuncG = _elm_lang$svg$Svg$node('feFuncG');
var _elm_lang$svg$Svg$feFuncR = _elm_lang$svg$Svg$node('feFuncR');
var _elm_lang$svg$Svg$feGaussianBlur = _elm_lang$svg$Svg$node('feGaussianBlur');
var _elm_lang$svg$Svg$feImage = _elm_lang$svg$Svg$node('feImage');
var _elm_lang$svg$Svg$feMerge = _elm_lang$svg$Svg$node('feMerge');
var _elm_lang$svg$Svg$feMergeNode = _elm_lang$svg$Svg$node('feMergeNode');
var _elm_lang$svg$Svg$feMorphology = _elm_lang$svg$Svg$node('feMorphology');
var _elm_lang$svg$Svg$feOffset = _elm_lang$svg$Svg$node('feOffset');
var _elm_lang$svg$Svg$feSpecularLighting = _elm_lang$svg$Svg$node('feSpecularLighting');
var _elm_lang$svg$Svg$feTile = _elm_lang$svg$Svg$node('feTile');
var _elm_lang$svg$Svg$feTurbulence = _elm_lang$svg$Svg$node('feTurbulence');
var _elm_lang$svg$Svg$font = _elm_lang$svg$Svg$node('font');
var _elm_lang$svg$Svg$linearGradient = _elm_lang$svg$Svg$node('linearGradient');
var _elm_lang$svg$Svg$radialGradient = _elm_lang$svg$Svg$node('radialGradient');
var _elm_lang$svg$Svg$stop = _elm_lang$svg$Svg$node('stop');
var _elm_lang$svg$Svg$circle = _elm_lang$svg$Svg$node('circle');
var _elm_lang$svg$Svg$ellipse = _elm_lang$svg$Svg$node('ellipse');
var _elm_lang$svg$Svg$image = _elm_lang$svg$Svg$node('image');
var _elm_lang$svg$Svg$line = _elm_lang$svg$Svg$node('line');
var _elm_lang$svg$Svg$path = _elm_lang$svg$Svg$node('path');
var _elm_lang$svg$Svg$polygon = _elm_lang$svg$Svg$node('polygon');
var _elm_lang$svg$Svg$polyline = _elm_lang$svg$Svg$node('polyline');
var _elm_lang$svg$Svg$rect = _elm_lang$svg$Svg$node('rect');
var _elm_lang$svg$Svg$use = _elm_lang$svg$Svg$node('use');
var _elm_lang$svg$Svg$feDistantLight = _elm_lang$svg$Svg$node('feDistantLight');
var _elm_lang$svg$Svg$fePointLight = _elm_lang$svg$Svg$node('fePointLight');
var _elm_lang$svg$Svg$feSpotLight = _elm_lang$svg$Svg$node('feSpotLight');
var _elm_lang$svg$Svg$altGlyph = _elm_lang$svg$Svg$node('altGlyph');
var _elm_lang$svg$Svg$altGlyphDef = _elm_lang$svg$Svg$node('altGlyphDef');
var _elm_lang$svg$Svg$altGlyphItem = _elm_lang$svg$Svg$node('altGlyphItem');
var _elm_lang$svg$Svg$glyph = _elm_lang$svg$Svg$node('glyph');
var _elm_lang$svg$Svg$glyphRef = _elm_lang$svg$Svg$node('glyphRef');
var _elm_lang$svg$Svg$textPath = _elm_lang$svg$Svg$node('textPath');
var _elm_lang$svg$Svg$text_ = _elm_lang$svg$Svg$node('text');
var _elm_lang$svg$Svg$tref = _elm_lang$svg$Svg$node('tref');
var _elm_lang$svg$Svg$tspan = _elm_lang$svg$Svg$node('tspan');
var _elm_lang$svg$Svg$clipPath = _elm_lang$svg$Svg$node('clipPath');
var _elm_lang$svg$Svg$colorProfile = _elm_lang$svg$Svg$node('colorProfile');
var _elm_lang$svg$Svg$cursor = _elm_lang$svg$Svg$node('cursor');
var _elm_lang$svg$Svg$filter = _elm_lang$svg$Svg$node('filter');
var _elm_lang$svg$Svg$script = _elm_lang$svg$Svg$node('script');
var _elm_lang$svg$Svg$style = _elm_lang$svg$Svg$node('style');
var _elm_lang$svg$Svg$view = _elm_lang$svg$Svg$node('view');

var _elm_lang$svg$Svg_Attributes$writingMode = _elm_lang$virtual_dom$VirtualDom$attribute('writing-mode');
var _elm_lang$svg$Svg_Attributes$wordSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('word-spacing');
var _elm_lang$svg$Svg_Attributes$visibility = _elm_lang$virtual_dom$VirtualDom$attribute('visibility');
var _elm_lang$svg$Svg_Attributes$unicodeBidi = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-bidi');
var _elm_lang$svg$Svg_Attributes$textRendering = _elm_lang$virtual_dom$VirtualDom$attribute('text-rendering');
var _elm_lang$svg$Svg_Attributes$textDecoration = _elm_lang$virtual_dom$VirtualDom$attribute('text-decoration');
var _elm_lang$svg$Svg_Attributes$textAnchor = _elm_lang$virtual_dom$VirtualDom$attribute('text-anchor');
var _elm_lang$svg$Svg_Attributes$stroke = _elm_lang$virtual_dom$VirtualDom$attribute('stroke');
var _elm_lang$svg$Svg_Attributes$strokeWidth = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-width');
var _elm_lang$svg$Svg_Attributes$strokeOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-opacity');
var _elm_lang$svg$Svg_Attributes$strokeMiterlimit = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-miterlimit');
var _elm_lang$svg$Svg_Attributes$strokeLinejoin = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linejoin');
var _elm_lang$svg$Svg_Attributes$strokeLinecap = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linecap');
var _elm_lang$svg$Svg_Attributes$strokeDashoffset = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dashoffset');
var _elm_lang$svg$Svg_Attributes$strokeDasharray = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dasharray');
var _elm_lang$svg$Svg_Attributes$stopOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stop-opacity');
var _elm_lang$svg$Svg_Attributes$stopColor = _elm_lang$virtual_dom$VirtualDom$attribute('stop-color');
var _elm_lang$svg$Svg_Attributes$shapeRendering = _elm_lang$virtual_dom$VirtualDom$attribute('shape-rendering');
var _elm_lang$svg$Svg_Attributes$pointerEvents = _elm_lang$virtual_dom$VirtualDom$attribute('pointer-events');
var _elm_lang$svg$Svg_Attributes$overflow = _elm_lang$virtual_dom$VirtualDom$attribute('overflow');
var _elm_lang$svg$Svg_Attributes$opacity = _elm_lang$virtual_dom$VirtualDom$attribute('opacity');
var _elm_lang$svg$Svg_Attributes$mask = _elm_lang$virtual_dom$VirtualDom$attribute('mask');
var _elm_lang$svg$Svg_Attributes$markerStart = _elm_lang$virtual_dom$VirtualDom$attribute('marker-start');
var _elm_lang$svg$Svg_Attributes$markerMid = _elm_lang$virtual_dom$VirtualDom$attribute('marker-mid');
var _elm_lang$svg$Svg_Attributes$markerEnd = _elm_lang$virtual_dom$VirtualDom$attribute('marker-end');
var _elm_lang$svg$Svg_Attributes$lightingColor = _elm_lang$virtual_dom$VirtualDom$attribute('lighting-color');
var _elm_lang$svg$Svg_Attributes$letterSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('letter-spacing');
var _elm_lang$svg$Svg_Attributes$kerning = _elm_lang$virtual_dom$VirtualDom$attribute('kerning');
var _elm_lang$svg$Svg_Attributes$imageRendering = _elm_lang$virtual_dom$VirtualDom$attribute('image-rendering');
var _elm_lang$svg$Svg_Attributes$glyphOrientationVertical = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-vertical');
var _elm_lang$svg$Svg_Attributes$glyphOrientationHorizontal = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-horizontal');
var _elm_lang$svg$Svg_Attributes$fontWeight = _elm_lang$virtual_dom$VirtualDom$attribute('font-weight');
var _elm_lang$svg$Svg_Attributes$fontVariant = _elm_lang$virtual_dom$VirtualDom$attribute('font-variant');
var _elm_lang$svg$Svg_Attributes$fontStyle = _elm_lang$virtual_dom$VirtualDom$attribute('font-style');
var _elm_lang$svg$Svg_Attributes$fontStretch = _elm_lang$virtual_dom$VirtualDom$attribute('font-stretch');
var _elm_lang$svg$Svg_Attributes$fontSize = _elm_lang$virtual_dom$VirtualDom$attribute('font-size');
var _elm_lang$svg$Svg_Attributes$fontSizeAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('font-size-adjust');
var _elm_lang$svg$Svg_Attributes$fontFamily = _elm_lang$virtual_dom$VirtualDom$attribute('font-family');
var _elm_lang$svg$Svg_Attributes$floodOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('flood-opacity');
var _elm_lang$svg$Svg_Attributes$floodColor = _elm_lang$virtual_dom$VirtualDom$attribute('flood-color');
var _elm_lang$svg$Svg_Attributes$filter = _elm_lang$virtual_dom$VirtualDom$attribute('filter');
var _elm_lang$svg$Svg_Attributes$fill = _elm_lang$virtual_dom$VirtualDom$attribute('fill');
var _elm_lang$svg$Svg_Attributes$fillRule = _elm_lang$virtual_dom$VirtualDom$attribute('fill-rule');
var _elm_lang$svg$Svg_Attributes$fillOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('fill-opacity');
var _elm_lang$svg$Svg_Attributes$enableBackground = _elm_lang$virtual_dom$VirtualDom$attribute('enable-background');
var _elm_lang$svg$Svg_Attributes$dominantBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('dominant-baseline');
var _elm_lang$svg$Svg_Attributes$display = _elm_lang$virtual_dom$VirtualDom$attribute('display');
var _elm_lang$svg$Svg_Attributes$direction = _elm_lang$virtual_dom$VirtualDom$attribute('direction');
var _elm_lang$svg$Svg_Attributes$cursor = _elm_lang$virtual_dom$VirtualDom$attribute('cursor');
var _elm_lang$svg$Svg_Attributes$color = _elm_lang$virtual_dom$VirtualDom$attribute('color');
var _elm_lang$svg$Svg_Attributes$colorRendering = _elm_lang$virtual_dom$VirtualDom$attribute('color-rendering');
var _elm_lang$svg$Svg_Attributes$colorProfile = _elm_lang$virtual_dom$VirtualDom$attribute('color-profile');
var _elm_lang$svg$Svg_Attributes$colorInterpolation = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation');
var _elm_lang$svg$Svg_Attributes$colorInterpolationFilters = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation-filters');
var _elm_lang$svg$Svg_Attributes$clip = _elm_lang$virtual_dom$VirtualDom$attribute('clip');
var _elm_lang$svg$Svg_Attributes$clipRule = _elm_lang$virtual_dom$VirtualDom$attribute('clip-rule');
var _elm_lang$svg$Svg_Attributes$clipPath = _elm_lang$virtual_dom$VirtualDom$attribute('clip-path');
var _elm_lang$svg$Svg_Attributes$baselineShift = _elm_lang$virtual_dom$VirtualDom$attribute('baseline-shift');
var _elm_lang$svg$Svg_Attributes$alignmentBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('alignment-baseline');
var _elm_lang$svg$Svg_Attributes$zoomAndPan = _elm_lang$virtual_dom$VirtualDom$attribute('zoomAndPan');
var _elm_lang$svg$Svg_Attributes$z = _elm_lang$virtual_dom$VirtualDom$attribute('z');
var _elm_lang$svg$Svg_Attributes$yChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('yChannelSelector');
var _elm_lang$svg$Svg_Attributes$y2 = _elm_lang$virtual_dom$VirtualDom$attribute('y2');
var _elm_lang$svg$Svg_Attributes$y1 = _elm_lang$virtual_dom$VirtualDom$attribute('y1');
var _elm_lang$svg$Svg_Attributes$y = _elm_lang$virtual_dom$VirtualDom$attribute('y');
var _elm_lang$svg$Svg_Attributes$xmlSpace = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:space');
var _elm_lang$svg$Svg_Attributes$xmlLang = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:lang');
var _elm_lang$svg$Svg_Attributes$xmlBase = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:base');
var _elm_lang$svg$Svg_Attributes$xlinkType = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:type');
var _elm_lang$svg$Svg_Attributes$xlinkTitle = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:title');
var _elm_lang$svg$Svg_Attributes$xlinkShow = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:show');
var _elm_lang$svg$Svg_Attributes$xlinkRole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:role');
var _elm_lang$svg$Svg_Attributes$xlinkHref = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:href');
var _elm_lang$svg$Svg_Attributes$xlinkArcrole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:arcrole');
var _elm_lang$svg$Svg_Attributes$xlinkActuate = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:actuate');
var _elm_lang$svg$Svg_Attributes$xChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('xChannelSelector');
var _elm_lang$svg$Svg_Attributes$x2 = _elm_lang$virtual_dom$VirtualDom$attribute('x2');
var _elm_lang$svg$Svg_Attributes$x1 = _elm_lang$virtual_dom$VirtualDom$attribute('x1');
var _elm_lang$svg$Svg_Attributes$xHeight = _elm_lang$virtual_dom$VirtualDom$attribute('x-height');
var _elm_lang$svg$Svg_Attributes$x = _elm_lang$virtual_dom$VirtualDom$attribute('x');
var _elm_lang$svg$Svg_Attributes$widths = _elm_lang$virtual_dom$VirtualDom$attribute('widths');
var _elm_lang$svg$Svg_Attributes$width = _elm_lang$virtual_dom$VirtualDom$attribute('width');
var _elm_lang$svg$Svg_Attributes$viewTarget = _elm_lang$virtual_dom$VirtualDom$attribute('viewTarget');
var _elm_lang$svg$Svg_Attributes$viewBox = _elm_lang$virtual_dom$VirtualDom$attribute('viewBox');
var _elm_lang$svg$Svg_Attributes$vertOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-y');
var _elm_lang$svg$Svg_Attributes$vertOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-x');
var _elm_lang$svg$Svg_Attributes$vertAdvY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-adv-y');
var _elm_lang$svg$Svg_Attributes$version = _elm_lang$virtual_dom$VirtualDom$attribute('version');
var _elm_lang$svg$Svg_Attributes$values = _elm_lang$virtual_dom$VirtualDom$attribute('values');
var _elm_lang$svg$Svg_Attributes$vMathematical = _elm_lang$virtual_dom$VirtualDom$attribute('v-mathematical');
var _elm_lang$svg$Svg_Attributes$vIdeographic = _elm_lang$virtual_dom$VirtualDom$attribute('v-ideographic');
var _elm_lang$svg$Svg_Attributes$vHanging = _elm_lang$virtual_dom$VirtualDom$attribute('v-hanging');
var _elm_lang$svg$Svg_Attributes$vAlphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('v-alphabetic');
var _elm_lang$svg$Svg_Attributes$unitsPerEm = _elm_lang$virtual_dom$VirtualDom$attribute('units-per-em');
var _elm_lang$svg$Svg_Attributes$unicodeRange = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-range');
var _elm_lang$svg$Svg_Attributes$unicode = _elm_lang$virtual_dom$VirtualDom$attribute('unicode');
var _elm_lang$svg$Svg_Attributes$underlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('underline-thickness');
var _elm_lang$svg$Svg_Attributes$underlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('underline-position');
var _elm_lang$svg$Svg_Attributes$u2 = _elm_lang$virtual_dom$VirtualDom$attribute('u2');
var _elm_lang$svg$Svg_Attributes$u1 = _elm_lang$virtual_dom$VirtualDom$attribute('u1');
var _elm_lang$svg$Svg_Attributes$type_ = _elm_lang$virtual_dom$VirtualDom$attribute('type');
var _elm_lang$svg$Svg_Attributes$transform = _elm_lang$virtual_dom$VirtualDom$attribute('transform');
var _elm_lang$svg$Svg_Attributes$to = _elm_lang$virtual_dom$VirtualDom$attribute('to');
var _elm_lang$svg$Svg_Attributes$title = _elm_lang$virtual_dom$VirtualDom$attribute('title');
var _elm_lang$svg$Svg_Attributes$textLength = _elm_lang$virtual_dom$VirtualDom$attribute('textLength');
var _elm_lang$svg$Svg_Attributes$targetY = _elm_lang$virtual_dom$VirtualDom$attribute('targetY');
var _elm_lang$svg$Svg_Attributes$targetX = _elm_lang$virtual_dom$VirtualDom$attribute('targetX');
var _elm_lang$svg$Svg_Attributes$target = _elm_lang$virtual_dom$VirtualDom$attribute('target');
var _elm_lang$svg$Svg_Attributes$tableValues = _elm_lang$virtual_dom$VirtualDom$attribute('tableValues');
var _elm_lang$svg$Svg_Attributes$systemLanguage = _elm_lang$virtual_dom$VirtualDom$attribute('systemLanguage');
var _elm_lang$svg$Svg_Attributes$surfaceScale = _elm_lang$virtual_dom$VirtualDom$attribute('surfaceScale');
var _elm_lang$svg$Svg_Attributes$style = _elm_lang$virtual_dom$VirtualDom$attribute('style');
var _elm_lang$svg$Svg_Attributes$string = _elm_lang$virtual_dom$VirtualDom$attribute('string');
var _elm_lang$svg$Svg_Attributes$strikethroughThickness = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-thickness');
var _elm_lang$svg$Svg_Attributes$strikethroughPosition = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-position');
var _elm_lang$svg$Svg_Attributes$stitchTiles = _elm_lang$virtual_dom$VirtualDom$attribute('stitchTiles');
var _elm_lang$svg$Svg_Attributes$stemv = _elm_lang$virtual_dom$VirtualDom$attribute('stemv');
var _elm_lang$svg$Svg_Attributes$stemh = _elm_lang$virtual_dom$VirtualDom$attribute('stemh');
var _elm_lang$svg$Svg_Attributes$stdDeviation = _elm_lang$virtual_dom$VirtualDom$attribute('stdDeviation');
var _elm_lang$svg$Svg_Attributes$startOffset = _elm_lang$virtual_dom$VirtualDom$attribute('startOffset');
var _elm_lang$svg$Svg_Attributes$spreadMethod = _elm_lang$virtual_dom$VirtualDom$attribute('spreadMethod');
var _elm_lang$svg$Svg_Attributes$speed = _elm_lang$virtual_dom$VirtualDom$attribute('speed');
var _elm_lang$svg$Svg_Attributes$specularExponent = _elm_lang$virtual_dom$VirtualDom$attribute('specularExponent');
var _elm_lang$svg$Svg_Attributes$specularConstant = _elm_lang$virtual_dom$VirtualDom$attribute('specularConstant');
var _elm_lang$svg$Svg_Attributes$spacing = _elm_lang$virtual_dom$VirtualDom$attribute('spacing');
var _elm_lang$svg$Svg_Attributes$slope = _elm_lang$virtual_dom$VirtualDom$attribute('slope');
var _elm_lang$svg$Svg_Attributes$seed = _elm_lang$virtual_dom$VirtualDom$attribute('seed');
var _elm_lang$svg$Svg_Attributes$scale = _elm_lang$virtual_dom$VirtualDom$attribute('scale');
var _elm_lang$svg$Svg_Attributes$ry = _elm_lang$virtual_dom$VirtualDom$attribute('ry');
var _elm_lang$svg$Svg_Attributes$rx = _elm_lang$virtual_dom$VirtualDom$attribute('rx');
var _elm_lang$svg$Svg_Attributes$rotate = _elm_lang$virtual_dom$VirtualDom$attribute('rotate');
var _elm_lang$svg$Svg_Attributes$result = _elm_lang$virtual_dom$VirtualDom$attribute('result');
var _elm_lang$svg$Svg_Attributes$restart = _elm_lang$virtual_dom$VirtualDom$attribute('restart');
var _elm_lang$svg$Svg_Attributes$requiredFeatures = _elm_lang$virtual_dom$VirtualDom$attribute('requiredFeatures');
var _elm_lang$svg$Svg_Attributes$requiredExtensions = _elm_lang$virtual_dom$VirtualDom$attribute('requiredExtensions');
var _elm_lang$svg$Svg_Attributes$repeatDur = _elm_lang$virtual_dom$VirtualDom$attribute('repeatDur');
var _elm_lang$svg$Svg_Attributes$repeatCount = _elm_lang$virtual_dom$VirtualDom$attribute('repeatCount');
var _elm_lang$svg$Svg_Attributes$renderingIntent = _elm_lang$virtual_dom$VirtualDom$attribute('rendering-intent');
var _elm_lang$svg$Svg_Attributes$refY = _elm_lang$virtual_dom$VirtualDom$attribute('refY');
var _elm_lang$svg$Svg_Attributes$refX = _elm_lang$virtual_dom$VirtualDom$attribute('refX');
var _elm_lang$svg$Svg_Attributes$radius = _elm_lang$virtual_dom$VirtualDom$attribute('radius');
var _elm_lang$svg$Svg_Attributes$r = _elm_lang$virtual_dom$VirtualDom$attribute('r');
var _elm_lang$svg$Svg_Attributes$primitiveUnits = _elm_lang$virtual_dom$VirtualDom$attribute('primitiveUnits');
var _elm_lang$svg$Svg_Attributes$preserveAspectRatio = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAspectRatio');
var _elm_lang$svg$Svg_Attributes$preserveAlpha = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAlpha');
var _elm_lang$svg$Svg_Attributes$pointsAtZ = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtZ');
var _elm_lang$svg$Svg_Attributes$pointsAtY = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtY');
var _elm_lang$svg$Svg_Attributes$pointsAtX = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtX');
var _elm_lang$svg$Svg_Attributes$points = _elm_lang$virtual_dom$VirtualDom$attribute('points');
var _elm_lang$svg$Svg_Attributes$pointOrder = _elm_lang$virtual_dom$VirtualDom$attribute('point-order');
var _elm_lang$svg$Svg_Attributes$patternUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternUnits');
var _elm_lang$svg$Svg_Attributes$patternTransform = _elm_lang$virtual_dom$VirtualDom$attribute('patternTransform');
var _elm_lang$svg$Svg_Attributes$patternContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternContentUnits');
var _elm_lang$svg$Svg_Attributes$pathLength = _elm_lang$virtual_dom$VirtualDom$attribute('pathLength');
var _elm_lang$svg$Svg_Attributes$path = _elm_lang$virtual_dom$VirtualDom$attribute('path');
var _elm_lang$svg$Svg_Attributes$panose1 = _elm_lang$virtual_dom$VirtualDom$attribute('panose-1');
var _elm_lang$svg$Svg_Attributes$overlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('overline-thickness');
var _elm_lang$svg$Svg_Attributes$overlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('overline-position');
var _elm_lang$svg$Svg_Attributes$origin = _elm_lang$virtual_dom$VirtualDom$attribute('origin');
var _elm_lang$svg$Svg_Attributes$orientation = _elm_lang$virtual_dom$VirtualDom$attribute('orientation');
var _elm_lang$svg$Svg_Attributes$orient = _elm_lang$virtual_dom$VirtualDom$attribute('orient');
var _elm_lang$svg$Svg_Attributes$order = _elm_lang$virtual_dom$VirtualDom$attribute('order');
var _elm_lang$svg$Svg_Attributes$operator = _elm_lang$virtual_dom$VirtualDom$attribute('operator');
var _elm_lang$svg$Svg_Attributes$offset = _elm_lang$virtual_dom$VirtualDom$attribute('offset');
var _elm_lang$svg$Svg_Attributes$numOctaves = _elm_lang$virtual_dom$VirtualDom$attribute('numOctaves');
var _elm_lang$svg$Svg_Attributes$name = _elm_lang$virtual_dom$VirtualDom$attribute('name');
var _elm_lang$svg$Svg_Attributes$mode = _elm_lang$virtual_dom$VirtualDom$attribute('mode');
var _elm_lang$svg$Svg_Attributes$min = _elm_lang$virtual_dom$VirtualDom$attribute('min');
var _elm_lang$svg$Svg_Attributes$method = _elm_lang$virtual_dom$VirtualDom$attribute('method');
var _elm_lang$svg$Svg_Attributes$media = _elm_lang$virtual_dom$VirtualDom$attribute('media');
var _elm_lang$svg$Svg_Attributes$max = _elm_lang$virtual_dom$VirtualDom$attribute('max');
var _elm_lang$svg$Svg_Attributes$mathematical = _elm_lang$virtual_dom$VirtualDom$attribute('mathematical');
var _elm_lang$svg$Svg_Attributes$maskUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskUnits');
var _elm_lang$svg$Svg_Attributes$maskContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskContentUnits');
var _elm_lang$svg$Svg_Attributes$markerWidth = _elm_lang$virtual_dom$VirtualDom$attribute('markerWidth');
var _elm_lang$svg$Svg_Attributes$markerUnits = _elm_lang$virtual_dom$VirtualDom$attribute('markerUnits');
var _elm_lang$svg$Svg_Attributes$markerHeight = _elm_lang$virtual_dom$VirtualDom$attribute('markerHeight');
var _elm_lang$svg$Svg_Attributes$local = _elm_lang$virtual_dom$VirtualDom$attribute('local');
var _elm_lang$svg$Svg_Attributes$limitingConeAngle = _elm_lang$virtual_dom$VirtualDom$attribute('limitingConeAngle');
var _elm_lang$svg$Svg_Attributes$lengthAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('lengthAdjust');
var _elm_lang$svg$Svg_Attributes$lang = _elm_lang$virtual_dom$VirtualDom$attribute('lang');
var _elm_lang$svg$Svg_Attributes$keyTimes = _elm_lang$virtual_dom$VirtualDom$attribute('keyTimes');
var _elm_lang$svg$Svg_Attributes$keySplines = _elm_lang$virtual_dom$VirtualDom$attribute('keySplines');
var _elm_lang$svg$Svg_Attributes$keyPoints = _elm_lang$virtual_dom$VirtualDom$attribute('keyPoints');
var _elm_lang$svg$Svg_Attributes$kernelUnitLength = _elm_lang$virtual_dom$VirtualDom$attribute('kernelUnitLength');
var _elm_lang$svg$Svg_Attributes$kernelMatrix = _elm_lang$virtual_dom$VirtualDom$attribute('kernelMatrix');
var _elm_lang$svg$Svg_Attributes$k4 = _elm_lang$virtual_dom$VirtualDom$attribute('k4');
var _elm_lang$svg$Svg_Attributes$k3 = _elm_lang$virtual_dom$VirtualDom$attribute('k3');
var _elm_lang$svg$Svg_Attributes$k2 = _elm_lang$virtual_dom$VirtualDom$attribute('k2');
var _elm_lang$svg$Svg_Attributes$k1 = _elm_lang$virtual_dom$VirtualDom$attribute('k1');
var _elm_lang$svg$Svg_Attributes$k = _elm_lang$virtual_dom$VirtualDom$attribute('k');
var _elm_lang$svg$Svg_Attributes$intercept = _elm_lang$virtual_dom$VirtualDom$attribute('intercept');
var _elm_lang$svg$Svg_Attributes$in2 = _elm_lang$virtual_dom$VirtualDom$attribute('in2');
var _elm_lang$svg$Svg_Attributes$in_ = _elm_lang$virtual_dom$VirtualDom$attribute('in');
var _elm_lang$svg$Svg_Attributes$ideographic = _elm_lang$virtual_dom$VirtualDom$attribute('ideographic');
var _elm_lang$svg$Svg_Attributes$id = _elm_lang$virtual_dom$VirtualDom$attribute('id');
var _elm_lang$svg$Svg_Attributes$horizOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-y');
var _elm_lang$svg$Svg_Attributes$horizOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-x');
var _elm_lang$svg$Svg_Attributes$horizAdvX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-adv-x');
var _elm_lang$svg$Svg_Attributes$height = _elm_lang$virtual_dom$VirtualDom$attribute('height');
var _elm_lang$svg$Svg_Attributes$hanging = _elm_lang$virtual_dom$VirtualDom$attribute('hanging');
var _elm_lang$svg$Svg_Attributes$gradientUnits = _elm_lang$virtual_dom$VirtualDom$attribute('gradientUnits');
var _elm_lang$svg$Svg_Attributes$gradientTransform = _elm_lang$virtual_dom$VirtualDom$attribute('gradientTransform');
var _elm_lang$svg$Svg_Attributes$glyphRef = _elm_lang$virtual_dom$VirtualDom$attribute('glyphRef');
var _elm_lang$svg$Svg_Attributes$glyphName = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-name');
var _elm_lang$svg$Svg_Attributes$g2 = _elm_lang$virtual_dom$VirtualDom$attribute('g2');
var _elm_lang$svg$Svg_Attributes$g1 = _elm_lang$virtual_dom$VirtualDom$attribute('g1');
var _elm_lang$svg$Svg_Attributes$fy = _elm_lang$virtual_dom$VirtualDom$attribute('fy');
var _elm_lang$svg$Svg_Attributes$fx = _elm_lang$virtual_dom$VirtualDom$attribute('fx');
var _elm_lang$svg$Svg_Attributes$from = _elm_lang$virtual_dom$VirtualDom$attribute('from');
var _elm_lang$svg$Svg_Attributes$format = _elm_lang$virtual_dom$VirtualDom$attribute('format');
var _elm_lang$svg$Svg_Attributes$filterUnits = _elm_lang$virtual_dom$VirtualDom$attribute('filterUnits');
var _elm_lang$svg$Svg_Attributes$filterRes = _elm_lang$virtual_dom$VirtualDom$attribute('filterRes');
var _elm_lang$svg$Svg_Attributes$externalResourcesRequired = _elm_lang$virtual_dom$VirtualDom$attribute('externalResourcesRequired');
var _elm_lang$svg$Svg_Attributes$exponent = _elm_lang$virtual_dom$VirtualDom$attribute('exponent');
var _elm_lang$svg$Svg_Attributes$end = _elm_lang$virtual_dom$VirtualDom$attribute('end');
var _elm_lang$svg$Svg_Attributes$elevation = _elm_lang$virtual_dom$VirtualDom$attribute('elevation');
var _elm_lang$svg$Svg_Attributes$edgeMode = _elm_lang$virtual_dom$VirtualDom$attribute('edgeMode');
var _elm_lang$svg$Svg_Attributes$dy = _elm_lang$virtual_dom$VirtualDom$attribute('dy');
var _elm_lang$svg$Svg_Attributes$dx = _elm_lang$virtual_dom$VirtualDom$attribute('dx');
var _elm_lang$svg$Svg_Attributes$dur = _elm_lang$virtual_dom$VirtualDom$attribute('dur');
var _elm_lang$svg$Svg_Attributes$divisor = _elm_lang$virtual_dom$VirtualDom$attribute('divisor');
var _elm_lang$svg$Svg_Attributes$diffuseConstant = _elm_lang$virtual_dom$VirtualDom$attribute('diffuseConstant');
var _elm_lang$svg$Svg_Attributes$descent = _elm_lang$virtual_dom$VirtualDom$attribute('descent');
var _elm_lang$svg$Svg_Attributes$decelerate = _elm_lang$virtual_dom$VirtualDom$attribute('decelerate');
var _elm_lang$svg$Svg_Attributes$d = _elm_lang$virtual_dom$VirtualDom$attribute('d');
var _elm_lang$svg$Svg_Attributes$cy = _elm_lang$virtual_dom$VirtualDom$attribute('cy');
var _elm_lang$svg$Svg_Attributes$cx = _elm_lang$virtual_dom$VirtualDom$attribute('cx');
var _elm_lang$svg$Svg_Attributes$contentStyleType = _elm_lang$virtual_dom$VirtualDom$attribute('contentStyleType');
var _elm_lang$svg$Svg_Attributes$contentScriptType = _elm_lang$virtual_dom$VirtualDom$attribute('contentScriptType');
var _elm_lang$svg$Svg_Attributes$clipPathUnits = _elm_lang$virtual_dom$VirtualDom$attribute('clipPathUnits');
var _elm_lang$svg$Svg_Attributes$class = _elm_lang$virtual_dom$VirtualDom$attribute('class');
var _elm_lang$svg$Svg_Attributes$capHeight = _elm_lang$virtual_dom$VirtualDom$attribute('cap-height');
var _elm_lang$svg$Svg_Attributes$calcMode = _elm_lang$virtual_dom$VirtualDom$attribute('calcMode');
var _elm_lang$svg$Svg_Attributes$by = _elm_lang$virtual_dom$VirtualDom$attribute('by');
var _elm_lang$svg$Svg_Attributes$bias = _elm_lang$virtual_dom$VirtualDom$attribute('bias');
var _elm_lang$svg$Svg_Attributes$begin = _elm_lang$virtual_dom$VirtualDom$attribute('begin');
var _elm_lang$svg$Svg_Attributes$bbox = _elm_lang$virtual_dom$VirtualDom$attribute('bbox');
var _elm_lang$svg$Svg_Attributes$baseProfile = _elm_lang$virtual_dom$VirtualDom$attribute('baseProfile');
var _elm_lang$svg$Svg_Attributes$baseFrequency = _elm_lang$virtual_dom$VirtualDom$attribute('baseFrequency');
var _elm_lang$svg$Svg_Attributes$azimuth = _elm_lang$virtual_dom$VirtualDom$attribute('azimuth');
var _elm_lang$svg$Svg_Attributes$autoReverse = _elm_lang$virtual_dom$VirtualDom$attribute('autoReverse');
var _elm_lang$svg$Svg_Attributes$attributeType = _elm_lang$virtual_dom$VirtualDom$attribute('attributeType');
var _elm_lang$svg$Svg_Attributes$attributeName = _elm_lang$virtual_dom$VirtualDom$attribute('attributeName');
var _elm_lang$svg$Svg_Attributes$ascent = _elm_lang$virtual_dom$VirtualDom$attribute('ascent');
var _elm_lang$svg$Svg_Attributes$arabicForm = _elm_lang$virtual_dom$VirtualDom$attribute('arabic-form');
var _elm_lang$svg$Svg_Attributes$amplitude = _elm_lang$virtual_dom$VirtualDom$attribute('amplitude');
var _elm_lang$svg$Svg_Attributes$allowReorder = _elm_lang$virtual_dom$VirtualDom$attribute('allowReorder');
var _elm_lang$svg$Svg_Attributes$alphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('alphabetic');
var _elm_lang$svg$Svg_Attributes$additive = _elm_lang$virtual_dom$VirtualDom$attribute('additive');
var _elm_lang$svg$Svg_Attributes$accumulate = _elm_lang$virtual_dom$VirtualDom$attribute('accumulate');
var _elm_lang$svg$Svg_Attributes$accelerate = _elm_lang$virtual_dom$VirtualDom$attribute('accelerate');
var _elm_lang$svg$Svg_Attributes$accentHeight = _elm_lang$virtual_dom$VirtualDom$attribute('accent-height');

var _elm_lang$svg$Svg_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$svg$Svg_Events$simpleOn = F2(
	function (name, msg) {
		return A2(
			_elm_lang$svg$Svg_Events$on,
			name,
			_elm_lang$core$Json_Decode$succeed(msg));
	});
var _elm_lang$svg$Svg_Events$onBegin = _elm_lang$svg$Svg_Events$simpleOn('begin');
var _elm_lang$svg$Svg_Events$onEnd = _elm_lang$svg$Svg_Events$simpleOn('end');
var _elm_lang$svg$Svg_Events$onRepeat = _elm_lang$svg$Svg_Events$simpleOn('repeat');
var _elm_lang$svg$Svg_Events$onAbort = _elm_lang$svg$Svg_Events$simpleOn('abort');
var _elm_lang$svg$Svg_Events$onError = _elm_lang$svg$Svg_Events$simpleOn('error');
var _elm_lang$svg$Svg_Events$onResize = _elm_lang$svg$Svg_Events$simpleOn('resize');
var _elm_lang$svg$Svg_Events$onScroll = _elm_lang$svg$Svg_Events$simpleOn('scroll');
var _elm_lang$svg$Svg_Events$onLoad = _elm_lang$svg$Svg_Events$simpleOn('load');
var _elm_lang$svg$Svg_Events$onUnload = _elm_lang$svg$Svg_Events$simpleOn('unload');
var _elm_lang$svg$Svg_Events$onZoom = _elm_lang$svg$Svg_Events$simpleOn('zoom');
var _elm_lang$svg$Svg_Events$onActivate = _elm_lang$svg$Svg_Events$simpleOn('activate');
var _elm_lang$svg$Svg_Events$onClick = _elm_lang$svg$Svg_Events$simpleOn('click');
var _elm_lang$svg$Svg_Events$onFocusIn = _elm_lang$svg$Svg_Events$simpleOn('focusin');
var _elm_lang$svg$Svg_Events$onFocusOut = _elm_lang$svg$Svg_Events$simpleOn('focusout');
var _elm_lang$svg$Svg_Events$onMouseDown = _elm_lang$svg$Svg_Events$simpleOn('mousedown');
var _elm_lang$svg$Svg_Events$onMouseMove = _elm_lang$svg$Svg_Events$simpleOn('mousemove');
var _elm_lang$svg$Svg_Events$onMouseOut = _elm_lang$svg$Svg_Events$simpleOn('mouseout');
var _elm_lang$svg$Svg_Events$onMouseOver = _elm_lang$svg$Svg_Events$simpleOn('mouseover');
var _elm_lang$svg$Svg_Events$onMouseUp = _elm_lang$svg$Svg_Events$simpleOn('mouseup');

var _elm_lang$svg$Svg_Lazy$lazy3 = _elm_lang$virtual_dom$VirtualDom$lazy3;
var _elm_lang$svg$Svg_Lazy$lazy2 = _elm_lang$virtual_dom$VirtualDom$lazy2;
var _elm_lang$svg$Svg_Lazy$lazy = _elm_lang$virtual_dom$VirtualDom$lazy;

var _fredcy$elm_parseint$ParseInt$charFromInt = function (i) {
	return (_elm_lang$core$Native_Utils.cmp(i, 10) < 0) ? _elm_lang$core$Char$fromCode(
		i + _elm_lang$core$Char$toCode(
			_elm_lang$core$Native_Utils.chr('0'))) : ((_elm_lang$core$Native_Utils.cmp(i, 36) < 0) ? _elm_lang$core$Char$fromCode(
		(i - 10) + _elm_lang$core$Char$toCode(
			_elm_lang$core$Native_Utils.chr('A'))) : _elm_lang$core$Native_Utils.crash(
		'ParseInt',
		{
			start: {line: 158, column: 9},
			end: {line: 158, column: 20}
		})(
		_elm_lang$core$Basics$toString(i)));
};
var _fredcy$elm_parseint$ParseInt$toRadixUnsafe = F2(
	function (radix, i) {
		return (_elm_lang$core$Native_Utils.cmp(i, radix) < 0) ? _elm_lang$core$String$fromChar(
			_fredcy$elm_parseint$ParseInt$charFromInt(i)) : A2(
			_elm_lang$core$Basics_ops['++'],
			A2(_fredcy$elm_parseint$ParseInt$toRadixUnsafe, radix, (i / radix) | 0),
			_elm_lang$core$String$fromChar(
				_fredcy$elm_parseint$ParseInt$charFromInt(
					A2(_elm_lang$core$Basics_ops['%'], i, radix))));
	});
var _fredcy$elm_parseint$ParseInt$toOct = _fredcy$elm_parseint$ParseInt$toRadixUnsafe(8);
var _fredcy$elm_parseint$ParseInt$toHex = _fredcy$elm_parseint$ParseInt$toRadixUnsafe(16);
var _fredcy$elm_parseint$ParseInt$isBetween = F3(
	function (lower, upper, c) {
		var ci = _elm_lang$core$Char$toCode(c);
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$Char$toCode(lower),
			ci) < 1) && (_elm_lang$core$Native_Utils.cmp(
			ci,
			_elm_lang$core$Char$toCode(upper)) < 1);
	});
var _fredcy$elm_parseint$ParseInt$charOffset = F2(
	function (basis, c) {
		return _elm_lang$core$Char$toCode(c) - _elm_lang$core$Char$toCode(basis);
	});
var _fredcy$elm_parseint$ParseInt$InvalidRadix = function (a) {
	return {ctor: 'InvalidRadix', _0: a};
};
var _fredcy$elm_parseint$ParseInt$toRadix = F2(
	function (radix, i) {
		return ((_elm_lang$core$Native_Utils.cmp(2, radix) < 1) && (_elm_lang$core$Native_Utils.cmp(radix, 36) < 1)) ? ((_elm_lang$core$Native_Utils.cmp(i, 0) < 0) ? _elm_lang$core$Result$Ok(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'-',
				A2(_fredcy$elm_parseint$ParseInt$toRadixUnsafe, radix, 0 - i))) : _elm_lang$core$Result$Ok(
			A2(_fredcy$elm_parseint$ParseInt$toRadixUnsafe, radix, i))) : _elm_lang$core$Result$Err(
			_fredcy$elm_parseint$ParseInt$InvalidRadix(radix));
	});
var _fredcy$elm_parseint$ParseInt$OutOfRange = function (a) {
	return {ctor: 'OutOfRange', _0: a};
};
var _fredcy$elm_parseint$ParseInt$InvalidChar = function (a) {
	return {ctor: 'InvalidChar', _0: a};
};
var _fredcy$elm_parseint$ParseInt$intFromChar = F2(
	function (radix, c) {
		var validInt = function (i) {
			return (_elm_lang$core$Native_Utils.cmp(i, radix) < 0) ? _elm_lang$core$Result$Ok(i) : _elm_lang$core$Result$Err(
				_fredcy$elm_parseint$ParseInt$OutOfRange(c));
		};
		var toInt = A3(
			_fredcy$elm_parseint$ParseInt$isBetween,
			_elm_lang$core$Native_Utils.chr('0'),
			_elm_lang$core$Native_Utils.chr('9'),
			c) ? _elm_lang$core$Result$Ok(
			A2(
				_fredcy$elm_parseint$ParseInt$charOffset,
				_elm_lang$core$Native_Utils.chr('0'),
				c)) : (A3(
			_fredcy$elm_parseint$ParseInt$isBetween,
			_elm_lang$core$Native_Utils.chr('a'),
			_elm_lang$core$Native_Utils.chr('z'),
			c) ? _elm_lang$core$Result$Ok(
			10 + A2(
				_fredcy$elm_parseint$ParseInt$charOffset,
				_elm_lang$core$Native_Utils.chr('a'),
				c)) : (A3(
			_fredcy$elm_parseint$ParseInt$isBetween,
			_elm_lang$core$Native_Utils.chr('A'),
			_elm_lang$core$Native_Utils.chr('Z'),
			c) ? _elm_lang$core$Result$Ok(
			10 + A2(
				_fredcy$elm_parseint$ParseInt$charOffset,
				_elm_lang$core$Native_Utils.chr('A'),
				c)) : _elm_lang$core$Result$Err(
			_fredcy$elm_parseint$ParseInt$InvalidChar(c))));
		return A2(_elm_lang$core$Result$andThen, validInt, toInt);
	});
var _fredcy$elm_parseint$ParseInt$parseIntR = F2(
	function (radix, rstring) {
		var _p0 = _elm_lang$core$String$uncons(rstring);
		if (_p0.ctor === 'Nothing') {
			return _elm_lang$core$Result$Ok(0);
		} else {
			return A2(
				_elm_lang$core$Result$andThen,
				function (ci) {
					return A2(
						_elm_lang$core$Result$andThen,
						function (ri) {
							return _elm_lang$core$Result$Ok(ci + (ri * radix));
						},
						A2(_fredcy$elm_parseint$ParseInt$parseIntR, radix, _p0._0._1));
				},
				A2(_fredcy$elm_parseint$ParseInt$intFromChar, radix, _p0._0._0));
		}
	});
var _fredcy$elm_parseint$ParseInt$parseIntRadix = F2(
	function (radix, string) {
		return ((_elm_lang$core$Native_Utils.cmp(2, radix) < 1) && (_elm_lang$core$Native_Utils.cmp(radix, 36) < 1)) ? A2(
			_fredcy$elm_parseint$ParseInt$parseIntR,
			radix,
			_elm_lang$core$String$reverse(string)) : _elm_lang$core$Result$Err(
			_fredcy$elm_parseint$ParseInt$InvalidRadix(radix));
	});
var _fredcy$elm_parseint$ParseInt$parseInt = _fredcy$elm_parseint$ParseInt$parseIntRadix(10);
var _fredcy$elm_parseint$ParseInt$parseIntOct = _fredcy$elm_parseint$ParseInt$parseIntRadix(8);
var _fredcy$elm_parseint$ParseInt$parseIntHex = _fredcy$elm_parseint$ParseInt$parseIntRadix(16);

var _eskimoblood$elm_color_extra$Color_Convert$xyzToColor = function (_p0) {
	var _p1 = _p0;
	var c = function (ch) {
		var ch_ = (_elm_lang$core$Native_Utils.cmp(ch, 3.1308e-3) > 0) ? ((1.055 * Math.pow(ch, 1 / 2.4)) - 5.5e-2) : (12.92 * ch);
		return _elm_lang$core$Basics$round(
			A3(_elm_lang$core$Basics$clamp, 0, 255, ch_ * 255));
	};
	var z_ = _p1.z / 100;
	var y_ = _p1.y / 100;
	var x_ = _p1.x / 100;
	var r = ((x_ * 3.2404542) + (y_ * -1.5371385)) + (z_ * -0.4986);
	var g = ((x_ * -0.969266) + (y_ * 1.8760108)) + (z_ * 4.1556e-2);
	var b = ((x_ * 5.56434e-2) + (y_ * -0.2040259)) + (z_ * 1.0572252);
	return A3(
		_elm_lang$core$Color$rgb,
		c(r),
		c(g),
		c(b));
};
var _eskimoblood$elm_color_extra$Color_Convert$labToXyz = function (_p2) {
	var _p3 = _p2;
	var y = (_p3.l + 16) / 116;
	var c = function (ch) {
		var ch_ = (ch * ch) * ch;
		return (_elm_lang$core$Native_Utils.cmp(ch_, 8.856e-3) > 0) ? ch_ : ((ch - (16 / 116)) / 7.787);
	};
	return {
		y: c(y) * 100,
		x: c(y + (_p3.a / 500)) * 95.047,
		z: c(y - (_p3.b / 200)) * 108.883
	};
};
var _eskimoblood$elm_color_extra$Color_Convert$labToColor = function (_p4) {
	return _eskimoblood$elm_color_extra$Color_Convert$xyzToColor(
		_eskimoblood$elm_color_extra$Color_Convert$labToXyz(_p4));
};
var _eskimoblood$elm_color_extra$Color_Convert$xyzToLab = function (_p5) {
	var _p6 = _p5;
	var c = function (ch) {
		return (_elm_lang$core$Native_Utils.cmp(ch, 8.856e-3) > 0) ? Math.pow(ch, 1 / 3) : ((7.787 * ch) + (16 / 116));
	};
	var x_ = c(_p6.x / 95.047);
	var y_ = c(_p6.y / 100);
	var z_ = c(_p6.z / 108.883);
	return {l: (116 * y_) - 16, a: 500 * (x_ - y_), b: 200 * (y_ - z_)};
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToXyz = function (cl) {
	var _p7 = _elm_lang$core$Color$toRgb(cl);
	var red = _p7.red;
	var green = _p7.green;
	var blue = _p7.blue;
	var c = function (ch) {
		var ch_ = _elm_lang$core$Basics$toFloat(ch) / 255;
		var ch__ = (_elm_lang$core$Native_Utils.cmp(ch_, 4.045e-2) > 0) ? Math.pow((ch_ + 5.5e-2) / 1.055, 2.4) : (ch_ / 12.92);
		return ch__ * 100;
	};
	var r = c(red);
	var g = c(green);
	var b = c(blue);
	return {x: ((r * 0.4124) + (g * 0.3576)) + (b * 0.1805), y: ((r * 0.2126) + (g * 0.7152)) + (b * 7.22e-2), z: ((r * 1.93e-2) + (g * 0.1192)) + (b * 0.9505)};
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToLab = function (_p8) {
	return _eskimoblood$elm_color_extra$Color_Convert$xyzToLab(
		_eskimoblood$elm_color_extra$Color_Convert$colorToXyz(_p8));
};
var _eskimoblood$elm_color_extra$Color_Convert$toRadix = function (n) {
	var getChr = function (c) {
		return (_elm_lang$core$Native_Utils.cmp(c, 10) < 0) ? _elm_lang$core$Basics$toString(c) : _elm_lang$core$String$fromChar(
			_elm_lang$core$Char$fromCode(87 + c));
	};
	return (_elm_lang$core$Native_Utils.cmp(n, 16) < 0) ? getChr(n) : A2(
		_elm_lang$core$Basics_ops['++'],
		_eskimoblood$elm_color_extra$Color_Convert$toRadix((n / 16) | 0),
		getChr(
			A2(_elm_lang$core$Basics_ops['%'], n, 16)));
};
var _eskimoblood$elm_color_extra$Color_Convert$toHex = function (_p9) {
	return A3(
		_elm_lang$core$String$padLeft,
		2,
		_elm_lang$core$Native_Utils.chr('0'),
		_eskimoblood$elm_color_extra$Color_Convert$toRadix(_p9));
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToHex = function (cl) {
	var _p10 = _elm_lang$core$Color$toRgb(cl);
	var red = _p10.red;
	var green = _p10.green;
	var blue = _p10.blue;
	return A2(
		_elm_lang$core$String$join,
		'',
		A2(
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			'#',
			A2(
				_elm_lang$core$List$map,
				_eskimoblood$elm_color_extra$Color_Convert$toHex,
				{
					ctor: '::',
					_0: red,
					_1: {
						ctor: '::',
						_0: green,
						_1: {
							ctor: '::',
							_0: blue,
							_1: {ctor: '[]'}
						}
					}
				})));
};
var _eskimoblood$elm_color_extra$Color_Convert$hexToColor = function () {
	var pattern = A2(
		_elm_lang$core$Basics_ops['++'],
		'',
		A2(
			_elm_lang$core$Basics_ops['++'],
			'^',
			A2(
				_elm_lang$core$Basics_ops['++'],
				'#?',
				A2(
					_elm_lang$core$Basics_ops['++'],
					'(?:',
					A2(
						_elm_lang$core$Basics_ops['++'],
						'(?:([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2}))',
						A2(
							_elm_lang$core$Basics_ops['++'],
							'|',
							A2(
								_elm_lang$core$Basics_ops['++'],
								'(?:([a-f\\d])([a-f\\d])([a-f\\d]))',
								A2(_elm_lang$core$Basics_ops['++'], ')', '$'))))))));
	var extend = function (token) {
		var _p11 = _elm_lang$core$String$toList(token);
		if ((_p11.ctor === '::') && (_p11._1.ctor === '[]')) {
			var _p12 = _p11._0;
			return _elm_lang$core$String$fromList(
				{
					ctor: '::',
					_0: _p12,
					_1: {
						ctor: '::',
						_0: _p12,
						_1: {ctor: '[]'}
					}
				});
		} else {
			return token;
		}
	};
	return function (_p13) {
		return A2(
			_elm_lang$core$Result$andThen,
			function (colors) {
				var _p15 = A2(
					_elm_lang$core$List$map,
					function (_p14) {
						return _fredcy$elm_parseint$ParseInt$parseIntHex(
							extend(_p14));
					},
					colors);
				if (((((((_p15.ctor === '::') && (_p15._0.ctor === 'Ok')) && (_p15._1.ctor === '::')) && (_p15._1._0.ctor === 'Ok')) && (_p15._1._1.ctor === '::')) && (_p15._1._1._0.ctor === 'Ok')) && (_p15._1._1._1.ctor === '[]')) {
					return _elm_lang$core$Result$Ok(
						A3(_elm_lang$core$Color$rgb, _p15._0._0, _p15._1._0._0, _p15._1._1._0._0));
				} else {
					return _elm_lang$core$Result$Err('Parsing ints from hex failed');
				}
			},
			A2(
				_elm_lang$core$Result$fromMaybe,
				'Parsing hex regex failed',
				A2(
					_elm_lang$core$Maybe$map,
					_elm_lang$core$List$filterMap(_elm_lang$core$Basics$identity),
					A2(
						_elm_lang$core$Maybe$map,
						function (_) {
							return _.submatches;
						},
						_elm_lang$core$List$head(
							A3(
								_elm_lang$core$Regex$find,
								_elm_lang$core$Regex$AtMost(1),
								_elm_lang$core$Regex$regex(pattern),
								_elm_lang$core$String$toLower(_p13)))))));
	};
}();
var _eskimoblood$elm_color_extra$Color_Convert$cssColorString = F2(
	function (kind, values) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			kind,
			A2(
				_elm_lang$core$Basics_ops['++'],
				'(',
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(_elm_lang$core$String$join, ', ', values),
					')')));
	});
var _eskimoblood$elm_color_extra$Color_Convert$toPercentString = function (_p16) {
	return A3(
		_elm_lang$core$Basics$flip,
		F2(
			function (x, y) {
				return A2(_elm_lang$core$Basics_ops['++'], x, y);
			}),
		'%',
		_elm_lang$core$Basics$toString(
			_elm_lang$core$Basics$round(
				A2(
					F2(
						function (x, y) {
							return x * y;
						}),
					100,
					_p16))));
};
var _eskimoblood$elm_color_extra$Color_Convert$hueToString = function (_p17) {
	return _elm_lang$core$Basics$toString(
		_elm_lang$core$Basics$round(
			A3(
				_elm_lang$core$Basics$flip,
				F2(
					function (x, y) {
						return x / y;
					}),
				_elm_lang$core$Basics$pi,
				A2(
					F2(
						function (x, y) {
							return x * y;
						}),
					180,
					_p17))));
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToCssHsla = function (cl) {
	var _p18 = _elm_lang$core$Color$toHsl(cl);
	var hue = _p18.hue;
	var saturation = _p18.saturation;
	var lightness = _p18.lightness;
	var alpha = _p18.alpha;
	return A2(
		_eskimoblood$elm_color_extra$Color_Convert$cssColorString,
		'hsla',
		{
			ctor: '::',
			_0: _eskimoblood$elm_color_extra$Color_Convert$hueToString(hue),
			_1: {
				ctor: '::',
				_0: _eskimoblood$elm_color_extra$Color_Convert$toPercentString(saturation),
				_1: {
					ctor: '::',
					_0: _eskimoblood$elm_color_extra$Color_Convert$toPercentString(lightness),
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Basics$toString(alpha),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToCssHsl = function (cl) {
	var _p19 = _elm_lang$core$Color$toHsl(cl);
	var hue = _p19.hue;
	var saturation = _p19.saturation;
	var lightness = _p19.lightness;
	var alpha = _p19.alpha;
	return A2(
		_eskimoblood$elm_color_extra$Color_Convert$cssColorString,
		'hsl',
		{
			ctor: '::',
			_0: _eskimoblood$elm_color_extra$Color_Convert$hueToString(hue),
			_1: {
				ctor: '::',
				_0: _eskimoblood$elm_color_extra$Color_Convert$toPercentString(saturation),
				_1: {
					ctor: '::',
					_0: _eskimoblood$elm_color_extra$Color_Convert$toPercentString(lightness),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToCssRgba = function (cl) {
	var _p20 = _elm_lang$core$Color$toRgb(cl);
	var red = _p20.red;
	var green = _p20.green;
	var blue = _p20.blue;
	var alpha = _p20.alpha;
	return A2(
		_eskimoblood$elm_color_extra$Color_Convert$cssColorString,
		'rgba',
		{
			ctor: '::',
			_0: _elm_lang$core$Basics$toString(red),
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Basics$toString(green),
				_1: {
					ctor: '::',
					_0: _elm_lang$core$Basics$toString(blue),
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Basics$toString(alpha),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _eskimoblood$elm_color_extra$Color_Convert$colorToCssRgb = function (cl) {
	var _p21 = _elm_lang$core$Color$toRgb(cl);
	var red = _p21.red;
	var green = _p21.green;
	var blue = _p21.blue;
	var alpha = _p21.alpha;
	return A2(
		_eskimoblood$elm_color_extra$Color_Convert$cssColorString,
		'rgb',
		{
			ctor: '::',
			_0: _elm_lang$core$Basics$toString(red),
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Basics$toString(green),
				_1: {
					ctor: '::',
					_0: _elm_lang$core$Basics$toString(blue),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _eskimoblood$elm_color_extra$Color_Convert$XYZ = F3(
	function (a, b, c) {
		return {x: a, y: b, z: c};
	});
var _eskimoblood$elm_color_extra$Color_Convert$Lab = F3(
	function (a, b, c) {
		return {l: a, a: b, b: c};
	});

var _jesseilev$graft$Graph_Extra$updateEdges = F2(
	function (updater, graph) {
		return A2(
			_elm_community$graph$Graph$fromNodesAndEdges,
			_elm_community$graph$Graph$nodes(graph),
			updater(
				_elm_community$graph$Graph$edges(graph)));
	});
var _jesseilev$graft$Graph_Extra$updateNodes = F2(
	function (updater, graph) {
		return A2(
			_elm_community$graph$Graph$fromNodesAndEdges,
			updater(
				_elm_community$graph$Graph$nodes(graph)),
			_elm_community$graph$Graph$edges(graph));
	});
var _jesseilev$graft$Graph_Extra$updateNode = F2(
	function (id, updater) {
		return _jesseilev$graft$Graph_Extra$updateNodes(
			_elm_lang$core$List$map(
				function (n) {
					return _elm_lang$core$Native_Utils.eq(n.id, id) ? updater(n) : n;
				}));
	});
var _jesseilev$graft$Graph_Extra$edgeEqualsFromTo = F3(
	function (from, to, edge) {
		return _elm_lang$core$Native_Utils.eq(edge.from, from) && _elm_lang$core$Native_Utils.eq(edge.to, to);
	});
var _jesseilev$graft$Graph_Extra$edgeEquals = function (edge) {
	return A2(_jesseilev$graft$Graph_Extra$edgeEqualsFromTo, edge.from, edge.to);
};
var _jesseilev$graft$Graph_Extra$insertEdge = F2(
	function (newEdge, graph) {
		var alreadyExists = A2(
			_elm_lang$core$Debug$log,
			'edge already exists?',
			_elm_community$maybe_extra$Maybe_Extra$isJust(
				A2(
					_elm_community$list_extra$List_Extra$find,
					_jesseilev$graft$Graph_Extra$edgeEquals(newEdge),
					_elm_community$graph$Graph$edges(graph))));
		var newEdges = alreadyExists ? {ctor: '[]'} : {
			ctor: '::',
			_0: newEdge,
			_1: {ctor: '[]'}
		};
		return A2(
			_elm_community$graph$Graph$fromNodesAndEdges,
			_elm_community$graph$Graph$nodes(graph),
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_community$graph$Graph$edges(graph),
				newEdges));
	});
var _jesseilev$graft$Graph_Extra$updateEdge = F3(
	function (from, to, updater) {
		return _jesseilev$graft$Graph_Extra$updateEdges(
			_elm_lang$core$List$map(
				function (e) {
					return A3(_jesseilev$graft$Graph_Extra$edgeEqualsFromTo, from, to, e) ? updater(e) : e;
				}));
	});
var _jesseilev$graft$Graph_Extra$neighborCount = F2(
	function (graph, node) {
		var adjCount = function (_p0) {
			return _elm_lang$core$List$length(
				_elm_community$intdict$IntDict$keys(_p0));
		};
		return A2(
			_elm_lang$core$Debug$log,
			'neighborcount',
			A2(
				_elm_lang$core$Maybe$withDefault,
				0,
				A2(
					_elm_lang$core$Maybe$map,
					function (_p1) {
						var _p2 = _p1;
						return adjCount(_p2.incoming) + adjCount(_p2.outgoing);
					},
					A2(_elm_community$graph$Graph$get, node.id, graph))));
	});
var _jesseilev$graft$Graph_Extra$getNode = function (nodeId) {
	return function (_p3) {
		return A2(
			_elm_lang$core$Maybe$map,
			function (_) {
				return _.node;
			},
			A2(_elm_community$graph$Graph$get, nodeId, _p3));
	};
};
var _jesseilev$graft$Graph_Extra$getEdge = F2(
	function (from, to) {
		return function (_p4) {
			return A2(
				_elm_community$list_extra$List_Extra$find,
				A2(_jesseilev$graft$Graph_Extra$edgeEqualsFromTo, from, to),
				_elm_community$graph$Graph$edges(_p4));
		};
	});

var _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d = function (a) {
	return {ctor: 'Vector2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d = function (a) {
	return {ctor: 'Vector3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Direction2d = function (a) {
	return {ctor: 'Direction2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d = function (a) {
	return {ctor: 'Direction3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Point2d = function (a) {
	return {ctor: 'Point2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Point3d = function (a) {
	return {ctor: 'Point3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d = function (a) {
	return {ctor: 'Axis2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d = function (a) {
	return {ctor: 'Axis3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Plane3d = function (a) {
	return {ctor: 'Plane3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Frame2d = function (a) {
	return {ctor: 'Frame2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Frame3d = function (a) {
	return {ctor: 'Frame3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d = function (a) {
	return {ctor: 'SketchPlane3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d = function (a) {
	return {ctor: 'LineSegment2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$LineSegment3d = function (a) {
	return {ctor: 'LineSegment3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Triangle2d = function (a) {
	return {ctor: 'Triangle2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Triangle3d = function (a) {
	return {ctor: 'Triangle3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$BoundingBox2d = function (a) {
	return {ctor: 'BoundingBox2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$BoundingBox3d = function (a) {
	return {ctor: 'BoundingBox3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Polyline2d = function (a) {
	return {ctor: 'Polyline2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Polyline3d = function (a) {
	return {ctor: 'Polyline3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Polygon2d = function (a) {
	return {ctor: 'Polygon2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Circle2d = function (a) {
	return {ctor: 'Circle2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Circle3d = function (a) {
	return {ctor: 'Circle3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Arc2d = function (a) {
	return {ctor: 'Arc2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$Arc3d = function (a) {
	return {ctor: 'Arc3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$QuadraticSpline2d = function (a) {
	return {ctor: 'QuadraticSpline2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$QuadraticSpline3d = function (a) {
	return {ctor: 'QuadraticSpline3d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$CubicSpline2d = function (a) {
	return {ctor: 'CubicSpline2d', _0: a};
};
var _opensolid$geometry$OpenSolid_Geometry_Types$CubicSpline3d = function (a) {
	return {ctor: 'CubicSpline3d', _0: a};
};

var _opensolid$geometry$OpenSolid_Scalar$interpolateFrom = F3(
	function (start, end, parameter) {
		return (_elm_lang$core$Native_Utils.cmp(parameter, 0.5) < 1) ? (start + (parameter * (end - start))) : (end + ((1 - parameter) * (start - end)));
	});
var _opensolid$geometry$OpenSolid_Scalar$equalWithin = F3(
	function (tolerance, firstValue, secondValue) {
		return _elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$Basics$abs(secondValue - firstValue),
			tolerance) < 1;
	});

var _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$components = function (_p0) {
	var _p1 = _p0;
	return _p1._0;
};
var _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$flip = function (direction) {
	var _p2 = _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$components(direction);
	var x = _p2._0;
	var y = _p2._1;
	return _opensolid$geometry$OpenSolid_Geometry_Types$Direction2d(
		{ctor: '_Tuple2', _0: 0 - x, _1: 0 - y});
};
var _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$perpendicularTo = function (direction) {
	var _p3 = _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$components(direction);
	var x = _p3._0;
	var y = _p3._1;
	return _opensolid$geometry$OpenSolid_Geometry_Types$Direction2d(
		{ctor: '_Tuple2', _0: 0 - y, _1: x});
};

var _opensolid$geometry$OpenSolid_Bootstrap_Axis2d$direction = function (_p0) {
	var _p1 = _p0;
	return _p1._0.direction;
};
var _opensolid$geometry$OpenSolid_Bootstrap_Axis2d$originPoint = function (_p2) {
	var _p3 = _p2;
	return _p3._0.originPoint;
};

var _opensolid$geometry$OpenSolid_Bootstrap_Frame2d$yDirection = function (_p0) {
	var _p1 = _p0;
	return _p1._0.yDirection;
};
var _opensolid$geometry$OpenSolid_Bootstrap_Frame2d$xDirection = function (_p2) {
	var _p3 = _p2;
	return _p3._0.xDirection;
};
var _opensolid$geometry$OpenSolid_Bootstrap_Frame2d$originPoint = function (_p4) {
	var _p5 = _p4;
	return _p5._0.originPoint;
};

var _opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$yDirection = function (_p0) {
	var _p1 = _p0;
	return _p1._0.yDirection;
};
var _opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$xDirection = function (_p2) {
	var _p3 = _p2;
	return _p3._0.xDirection;
};
var _opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$originPoint = function (_p4) {
	var _p5 = _p4;
	return _p5._0.originPoint;
};

var _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components = function (_p0) {
	var _p1 = _p0;
	return _p1._0;
};

var _opensolid$geometry$OpenSolid_Vector2d$placeIn = function (frame) {
	var _p0 = _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$components(
		_opensolid$geometry$OpenSolid_Bootstrap_Frame2d$yDirection(frame));
	var x2 = _p0._0;
	var y2 = _p0._1;
	var _p1 = _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$components(
		_opensolid$geometry$OpenSolid_Bootstrap_Frame2d$xDirection(frame));
	var x1 = _p1._0;
	var y1 = _p1._1;
	return function (_p2) {
		var _p3 = _p2;
		var _p5 = _p3._0._1;
		var _p4 = _p3._0._0;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
			{ctor: '_Tuple2', _0: (x1 * _p4) + (x2 * _p5), _1: (y1 * _p4) + (y2 * _p5)});
	};
};
var _opensolid$geometry$OpenSolid_Vector2d$mirrorAcross = function (axis) {
	var _p6 = _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$components(
		_opensolid$geometry$OpenSolid_Bootstrap_Axis2d$direction(axis));
	var dx = _p6._0;
	var dy = _p6._1;
	var a = 1 - ((2 * dy) * dy);
	var b = (2 * dx) * dy;
	var c = 1 - ((2 * dx) * dx);
	return function (_p7) {
		var _p8 = _p7;
		var _p10 = _p8._0._1;
		var _p9 = _p8._0._0;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
			{ctor: '_Tuple2', _0: (a * _p9) + (b * _p10), _1: (c * _p10) + (b * _p9)});
	};
};
var _opensolid$geometry$OpenSolid_Vector2d$rotateBy = function (angle) {
	var sine = _elm_lang$core$Basics$sin(angle);
	var cosine = _elm_lang$core$Basics$cos(angle);
	return function (_p11) {
		var _p12 = _p11;
		var _p14 = _p12._0._1;
		var _p13 = _p12._0._0;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
			{ctor: '_Tuple2', _0: (_p13 * cosine) - (_p14 * sine), _1: (_p14 * cosine) + (_p13 * sine)});
	};
};
var _opensolid$geometry$OpenSolid_Vector2d$yComponent = function (_p15) {
	var _p16 = _p15;
	return _p16._0._1;
};
var _opensolid$geometry$OpenSolid_Vector2d$xComponent = function (_p17) {
	var _p18 = _p17;
	return _p18._0._0;
};
var _opensolid$geometry$OpenSolid_Vector2d$components = function (_p19) {
	var _p20 = _p19;
	return _p20._0;
};
var _opensolid$geometry$OpenSolid_Vector2d$componentIn = F2(
	function (direction, vector) {
		var _p21 = _opensolid$geometry$OpenSolid_Vector2d$components(vector);
		var vx = _p21._0;
		var vy = _p21._1;
		var _p22 = _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$components(direction);
		var dx = _p22._0;
		var dy = _p22._1;
		return (vx * dx) + (vy * dy);
	});
var _opensolid$geometry$OpenSolid_Vector2d$relativeTo = F2(
	function (frame, vector) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
			{
				ctor: '_Tuple2',
				_0: A2(
					_opensolid$geometry$OpenSolid_Vector2d$componentIn,
					_opensolid$geometry$OpenSolid_Bootstrap_Frame2d$xDirection(frame),
					vector),
				_1: A2(
					_opensolid$geometry$OpenSolid_Vector2d$componentIn,
					_opensolid$geometry$OpenSolid_Bootstrap_Frame2d$yDirection(frame),
					vector)
			});
	});
var _opensolid$geometry$OpenSolid_Vector2d$squaredLength = function (vector) {
	var _p23 = _opensolid$geometry$OpenSolid_Vector2d$components(vector);
	var x = _p23._0;
	var y = _p23._1;
	return (x * x) + (y * y);
};
var _opensolid$geometry$OpenSolid_Vector2d$length = function (vector) {
	return _elm_lang$core$Basics$sqrt(
		_opensolid$geometry$OpenSolid_Vector2d$squaredLength(vector));
};
var _opensolid$geometry$OpenSolid_Vector2d$sum = F2(
	function (firstVector, secondVector) {
		var _p24 = _opensolid$geometry$OpenSolid_Vector2d$components(secondVector);
		var x2 = _p24._0;
		var y2 = _p24._1;
		var _p25 = _opensolid$geometry$OpenSolid_Vector2d$components(firstVector);
		var x1 = _p25._0;
		var y1 = _p25._1;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
			{ctor: '_Tuple2', _0: x1 + x2, _1: y1 + y2});
	});
var _opensolid$geometry$OpenSolid_Vector2d$difference = F2(
	function (firstVector, secondVector) {
		var _p26 = _opensolid$geometry$OpenSolid_Vector2d$components(secondVector);
		var x2 = _p26._0;
		var y2 = _p26._1;
		var _p27 = _opensolid$geometry$OpenSolid_Vector2d$components(firstVector);
		var x1 = _p27._0;
		var y1 = _p27._1;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
			{ctor: '_Tuple2', _0: x1 - x2, _1: y1 - y2});
	});
var _opensolid$geometry$OpenSolid_Vector2d$equalWithin = F3(
	function (tolerance, firstVector, secondVector) {
		return _elm_lang$core$Native_Utils.cmp(
			_opensolid$geometry$OpenSolid_Vector2d$squaredLength(
				A2(_opensolid$geometry$OpenSolid_Vector2d$difference, firstVector, secondVector)),
			tolerance * tolerance) < 1;
	});
var _opensolid$geometry$OpenSolid_Vector2d$dotProduct = F2(
	function (firstVector, secondVector) {
		var _p28 = _opensolid$geometry$OpenSolid_Vector2d$components(secondVector);
		var x2 = _p28._0;
		var y2 = _p28._1;
		var _p29 = _opensolid$geometry$OpenSolid_Vector2d$components(firstVector);
		var x1 = _p29._0;
		var y1 = _p29._1;
		return (x1 * x2) + (y1 * y2);
	});
var _opensolid$geometry$OpenSolid_Vector2d$crossProduct = F2(
	function (firstVector, secondVector) {
		var _p30 = _opensolid$geometry$OpenSolid_Vector2d$components(secondVector);
		var x2 = _p30._0;
		var y2 = _p30._1;
		var _p31 = _opensolid$geometry$OpenSolid_Vector2d$components(firstVector);
		var x1 = _p31._0;
		var y1 = _p31._1;
		return (x1 * y2) - (y1 * x2);
	});
var _opensolid$geometry$OpenSolid_Vector2d$flip = function (vector) {
	var _p32 = _opensolid$geometry$OpenSolid_Vector2d$components(vector);
	var x = _p32._0;
	var y = _p32._1;
	return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
		{ctor: '_Tuple2', _0: 0 - x, _1: 0 - y});
};
var _opensolid$geometry$OpenSolid_Vector2d$scaleBy = F2(
	function (scale, vector) {
		var _p33 = _opensolid$geometry$OpenSolid_Vector2d$components(vector);
		var x = _p33._0;
		var y = _p33._1;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
			{ctor: '_Tuple2', _0: x * scale, _1: y * scale});
	});
var _opensolid$geometry$OpenSolid_Vector2d$lengthAndDirection = function (vector) {
	var vectorLength = _opensolid$geometry$OpenSolid_Vector2d$length(vector);
	if (_elm_lang$core$Native_Utils.eq(vectorLength, 0.0)) {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		var normalizedVector = A2(_opensolid$geometry$OpenSolid_Vector2d$scaleBy, 1 / vectorLength, vector);
		var vectorDirection = _opensolid$geometry$OpenSolid_Geometry_Types$Direction2d(
			_opensolid$geometry$OpenSolid_Vector2d$components(normalizedVector));
		return _elm_lang$core$Maybe$Just(
			{ctor: '_Tuple2', _0: vectorLength, _1: vectorDirection});
	}
};
var _opensolid$geometry$OpenSolid_Vector2d$placeOnto = F2(
	function (sketchPlane, vector) {
		var _p34 = _opensolid$geometry$OpenSolid_Vector2d$components(vector);
		var x = _p34._0;
		var y = _p34._1;
		var _p35 = _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components(
			_opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$yDirection(sketchPlane));
		var vx = _p35._0;
		var vy = _p35._1;
		var vz = _p35._2;
		var _p36 = _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components(
			_opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$xDirection(sketchPlane));
		var ux = _p36._0;
		var uy = _p36._1;
		var uz = _p36._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
			{ctor: '_Tuple3', _0: (x * ux) + (y * vx), _1: (x * uy) + (y * vy), _2: (x * uz) + (y * vz)});
	});
var _opensolid$geometry$OpenSolid_Vector2d$interpolateFrom = F3(
	function (v1, v2, t) {
		var _p37 = _opensolid$geometry$OpenSolid_Vector2d$components(v2);
		var x2 = _p37._0;
		var y2 = _p37._1;
		var _p38 = _opensolid$geometry$OpenSolid_Vector2d$components(v1);
		var x1 = _p38._0;
		var y1 = _p38._1;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
			{
				ctor: '_Tuple2',
				_0: A3(_opensolid$geometry$OpenSolid_Scalar$interpolateFrom, x1, x2, t),
				_1: A3(_opensolid$geometry$OpenSolid_Scalar$interpolateFrom, y1, y2, t)
			});
	});
var _opensolid$geometry$OpenSolid_Vector2d$perpendicularTo = function (vector) {
	var _p39 = _opensolid$geometry$OpenSolid_Vector2d$components(vector);
	var x = _p39._0;
	var y = _p39._1;
	return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
		{ctor: '_Tuple2', _0: 0 - y, _1: x});
};
var _opensolid$geometry$OpenSolid_Vector2d$in_ = F2(
	function (direction, length) {
		var _p40 = _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$components(direction);
		var dx = _p40._0;
		var dy = _p40._1;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
			{ctor: '_Tuple2', _0: length * dx, _1: length * dy});
	});
var _opensolid$geometry$OpenSolid_Vector2d$projectionIn = F2(
	function (direction, vector) {
		return A2(
			_opensolid$geometry$OpenSolid_Vector2d$in_,
			direction,
			A2(_opensolid$geometry$OpenSolid_Vector2d$componentIn, direction, vector));
	});
var _opensolid$geometry$OpenSolid_Vector2d$projectOnto = F2(
	function (axis, vector) {
		return A2(
			_opensolid$geometry$OpenSolid_Vector2d$projectionIn,
			_opensolid$geometry$OpenSolid_Bootstrap_Axis2d$direction(axis),
			vector);
	});
var _opensolid$geometry$OpenSolid_Vector2d$polar = function (coordinates) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
		_elm_lang$core$Basics$fromPolar(coordinates));
};
var _opensolid$geometry$OpenSolid_Vector2d$zero = _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
	{ctor: '_Tuple2', _0: 0, _1: 0});
var _opensolid$geometry$OpenSolid_Vector2d$direction = function (vector) {
	if (_elm_lang$core$Native_Utils.eq(vector, _opensolid$geometry$OpenSolid_Vector2d$zero)) {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		var normalizedVector = A2(
			_opensolid$geometry$OpenSolid_Vector2d$scaleBy,
			1 / _opensolid$geometry$OpenSolid_Vector2d$length(vector),
			vector);
		return _elm_lang$core$Maybe$Just(
			_opensolid$geometry$OpenSolid_Geometry_Types$Direction2d(
				_opensolid$geometry$OpenSolid_Vector2d$components(normalizedVector)));
	}
};
var _opensolid$geometry$OpenSolid_Vector2d$orthonormalize = function (_p41) {
	var _p42 = _p41;
	return A2(
		_elm_lang$core$Maybe$andThen,
		function (xDirection) {
			var yDirection = _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$perpendicularTo(xDirection);
			var perpendicularComponent = A2(_opensolid$geometry$OpenSolid_Vector2d$componentIn, yDirection, _p42._1);
			return (_elm_lang$core$Native_Utils.cmp(perpendicularComponent, 0.0) > 0) ? _elm_lang$core$Maybe$Just(
				{ctor: '_Tuple2', _0: xDirection, _1: yDirection}) : ((_elm_lang$core$Native_Utils.cmp(perpendicularComponent, 0.0) < 0) ? _elm_lang$core$Maybe$Just(
				{
					ctor: '_Tuple2',
					_0: xDirection,
					_1: _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$flip(yDirection)
				}) : _elm_lang$core$Maybe$Nothing);
		},
		_opensolid$geometry$OpenSolid_Vector2d$direction(_p42._0));
};

var _opensolid$geometry$OpenSolid_Direction2d$flip = _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$flip;
var _opensolid$geometry$OpenSolid_Direction2d$toVector = function (_p0) {
	var _p1 = _p0;
	return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(_p1._0);
};
var _opensolid$geometry$OpenSolid_Direction2d$scaleBy = F2(
	function (scale, direction) {
		return A2(
			_opensolid$geometry$OpenSolid_Vector2d$scaleBy,
			scale,
			_opensolid$geometry$OpenSolid_Direction2d$toVector(direction));
	});
var _opensolid$geometry$OpenSolid_Direction2d$componentIn = F2(
	function (firstDirection, secondDirection) {
		return A2(
			_opensolid$geometry$OpenSolid_Vector2d$componentIn,
			firstDirection,
			_opensolid$geometry$OpenSolid_Direction2d$toVector(secondDirection));
	});
var _opensolid$geometry$OpenSolid_Direction2d$yComponent = function (_p2) {
	var _p3 = _p2;
	return _p3._0._1;
};
var _opensolid$geometry$OpenSolid_Direction2d$xComponent = function (_p4) {
	var _p5 = _p4;
	return _p5._0._0;
};
var _opensolid$geometry$OpenSolid_Direction2d$components = _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$components;
var _opensolid$geometry$OpenSolid_Direction2d$placeOnto = F2(
	function (sketchPlane, direction) {
		var _p6 = _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components(
			_opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$yDirection(sketchPlane));
		var vx = _p6._0;
		var vy = _p6._1;
		var vz = _p6._2;
		var _p7 = _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components(
			_opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$xDirection(sketchPlane));
		var ux = _p7._0;
		var uy = _p7._1;
		var uz = _p7._2;
		var _p8 = _opensolid$geometry$OpenSolid_Direction2d$components(direction);
		var x = _p8._0;
		var y = _p8._1;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
			{ctor: '_Tuple3', _0: (x * ux) + (y * vx), _1: (x * uy) + (y * vy), _2: (x * uz) + (y * vz)});
	});
var _opensolid$geometry$OpenSolid_Direction2d$angleFrom = F2(
	function (firstDirection, secondDirection) {
		var secondVector = _opensolid$geometry$OpenSolid_Direction2d$toVector(secondDirection);
		var firstVector = _opensolid$geometry$OpenSolid_Direction2d$toVector(firstDirection);
		return A2(
			_elm_lang$core$Basics$atan2,
			A2(_opensolid$geometry$OpenSolid_Vector2d$crossProduct, firstVector, secondVector),
			A2(_opensolid$geometry$OpenSolid_Vector2d$dotProduct, firstVector, secondVector));
	});
var _opensolid$geometry$OpenSolid_Direction2d$equalWithin = F3(
	function (angle, firstDirection, secondDirection) {
		return _elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$Basics$abs(
				A2(_opensolid$geometry$OpenSolid_Direction2d$angleFrom, firstDirection, secondDirection)),
			angle) < 1;
	});
var _opensolid$geometry$OpenSolid_Direction2d$toAngle = function (direction) {
	var _p9 = _opensolid$geometry$OpenSolid_Direction2d$components(direction);
	var x = _p9._0;
	var y = _p9._1;
	return A2(_elm_lang$core$Basics$atan2, y, x);
};
var _opensolid$geometry$OpenSolid_Direction2d$fromAngle = function (angle) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Direction2d(
		{
			ctor: '_Tuple2',
			_0: _elm_lang$core$Basics$cos(angle),
			_1: _elm_lang$core$Basics$sin(angle)
		});
};
var _opensolid$geometry$OpenSolid_Direction2d$orthogonalize = function (_p10) {
	var _p11 = _p10;
	return _opensolid$geometry$OpenSolid_Vector2d$orthonormalize(
		{
			ctor: '_Tuple2',
			_0: _opensolid$geometry$OpenSolid_Direction2d$toVector(_p11._0),
			_1: _opensolid$geometry$OpenSolid_Direction2d$toVector(_p11._1)
		});
};
var _opensolid$geometry$OpenSolid_Direction2d$perpendicularTo = _opensolid$geometry$OpenSolid_Bootstrap_Direction2d$perpendicularTo;
var _opensolid$geometry$OpenSolid_Direction2d$negativeY = _opensolid$geometry$OpenSolid_Geometry_Types$Direction2d(
	{ctor: '_Tuple2', _0: 0, _1: -1});
var _opensolid$geometry$OpenSolid_Direction2d$positiveY = _opensolid$geometry$OpenSolid_Geometry_Types$Direction2d(
	{ctor: '_Tuple2', _0: 0, _1: 1});
var _opensolid$geometry$OpenSolid_Direction2d$negativeX = _opensolid$geometry$OpenSolid_Geometry_Types$Direction2d(
	{ctor: '_Tuple2', _0: -1, _1: 0});
var _opensolid$geometry$OpenSolid_Direction2d$positiveX = _opensolid$geometry$OpenSolid_Geometry_Types$Direction2d(
	{ctor: '_Tuple2', _0: 1, _1: 0});
var _opensolid$geometry$OpenSolid_Direction2d$y = _opensolid$geometry$OpenSolid_Geometry_Types$Direction2d(
	{ctor: '_Tuple2', _0: 0, _1: 1});
var _opensolid$geometry$OpenSolid_Direction2d$x = _opensolid$geometry$OpenSolid_Geometry_Types$Direction2d(
	{ctor: '_Tuple2', _0: 1, _1: 0});
var _opensolid$geometry$OpenSolid_Direction2d$toDirection = function (_p12) {
	var _p13 = _p12;
	return _opensolid$geometry$OpenSolid_Geometry_Types$Direction2d(_p13._0);
};
var _opensolid$geometry$OpenSolid_Direction2d$rotateBy = F2(
	function (angle, direction) {
		return _opensolid$geometry$OpenSolid_Direction2d$toDirection(
			A2(
				_opensolid$geometry$OpenSolid_Vector2d$rotateBy,
				angle,
				_opensolid$geometry$OpenSolid_Direction2d$toVector(direction)));
	});
var _opensolid$geometry$OpenSolid_Direction2d$mirrorAcross = F2(
	function (axis, direction) {
		return _opensolid$geometry$OpenSolid_Direction2d$toDirection(
			A2(
				_opensolid$geometry$OpenSolid_Vector2d$mirrorAcross,
				axis,
				_opensolid$geometry$OpenSolid_Direction2d$toVector(direction)));
	});
var _opensolid$geometry$OpenSolid_Direction2d$relativeTo = F2(
	function (frame, direction) {
		return _opensolid$geometry$OpenSolid_Direction2d$toDirection(
			A2(
				_opensolid$geometry$OpenSolid_Vector2d$relativeTo,
				frame,
				_opensolid$geometry$OpenSolid_Direction2d$toVector(direction)));
	});
var _opensolid$geometry$OpenSolid_Direction2d$placeIn = F2(
	function (frame, direction) {
		return _opensolid$geometry$OpenSolid_Direction2d$toDirection(
			A2(
				_opensolid$geometry$OpenSolid_Vector2d$placeIn,
				frame,
				_opensolid$geometry$OpenSolid_Direction2d$toVector(direction)));
	});

var _opensolid$geometry$OpenSolid_Bootstrap_Point3d$coordinates = function (_p0) {
	var _p1 = _p0;
	return _p1._0;
};

var _opensolid$geometry$OpenSolid_Point2d$placeOnto = function (sketchPlane) {
	var _p0 = _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components(
		_opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$yDirection(sketchPlane));
	var vx = _p0._0;
	var vy = _p0._1;
	var vz = _p0._2;
	var _p1 = _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components(
		_opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$xDirection(sketchPlane));
	var ux = _p1._0;
	var uy = _p1._1;
	var uz = _p1._2;
	var _p2 = _opensolid$geometry$OpenSolid_Bootstrap_Point3d$coordinates(
		_opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$originPoint(sketchPlane));
	var x0 = _p2._0;
	var y0 = _p2._1;
	var z0 = _p2._2;
	return function (_p3) {
		var _p4 = _p3;
		var _p6 = _p4._0._1;
		var _p5 = _p4._0._0;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Point3d(
			{ctor: '_Tuple3', _0: (x0 + (_p5 * ux)) + (_p6 * vx), _1: (y0 + (_p5 * uy)) + (_p6 * vy), _2: (z0 + (_p5 * uz)) + (_p6 * vz)});
	};
};
var _opensolid$geometry$OpenSolid_Point2d$yCoordinate = function (_p7) {
	var _p8 = _p7;
	return _p8._0._1;
};
var _opensolid$geometry$OpenSolid_Point2d$xCoordinate = function (_p9) {
	var _p10 = _p9;
	return _p10._0._0;
};
var _opensolid$geometry$OpenSolid_Point2d$coordinates = function (_p11) {
	var _p12 = _p11;
	return _p12._0;
};
var _opensolid$geometry$OpenSolid_Point2d$vectorFrom = F2(
	function (firstPoint, secondPoint) {
		var _p13 = _opensolid$geometry$OpenSolid_Point2d$coordinates(secondPoint);
		var x2 = _p13._0;
		var y2 = _p13._1;
		var _p14 = _opensolid$geometry$OpenSolid_Point2d$coordinates(firstPoint);
		var x1 = _p14._0;
		var y1 = _p14._1;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
			{ctor: '_Tuple2', _0: x2 - x1, _1: y2 - y1});
	});
var _opensolid$geometry$OpenSolid_Point2d$directionFrom = F2(
	function (firstPoint, secondPoint) {
		return _opensolid$geometry$OpenSolid_Vector2d$direction(
			A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, firstPoint, secondPoint));
	});
var _opensolid$geometry$OpenSolid_Point2d$squaredDistanceFrom = F2(
	function (firstPoint, secondPoint) {
		return _opensolid$geometry$OpenSolid_Vector2d$squaredLength(
			A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, firstPoint, secondPoint));
	});
var _opensolid$geometry$OpenSolid_Point2d$equalWithin = F3(
	function (tolerance, firstPoint, secondPoint) {
		return _elm_lang$core$Native_Utils.cmp(
			A2(_opensolid$geometry$OpenSolid_Point2d$squaredDistanceFrom, firstPoint, secondPoint),
			tolerance * tolerance) < 1;
	});
var _opensolid$geometry$OpenSolid_Point2d$distanceFrom = F2(
	function (firstPoint, secondPoint) {
		return _elm_lang$core$Basics$sqrt(
			A2(_opensolid$geometry$OpenSolid_Point2d$squaredDistanceFrom, firstPoint, secondPoint));
	});
var _opensolid$geometry$OpenSolid_Point2d$distanceAlong = F2(
	function (axis, point) {
		return A2(
			_opensolid$geometry$OpenSolid_Vector2d$componentIn,
			_opensolid$geometry$OpenSolid_Bootstrap_Axis2d$direction(axis),
			A2(
				_opensolid$geometry$OpenSolid_Point2d$vectorFrom,
				_opensolid$geometry$OpenSolid_Bootstrap_Axis2d$originPoint(axis),
				point));
	});
var _opensolid$geometry$OpenSolid_Point2d$signedDistanceFrom = F2(
	function (axis, point) {
		var displacementVector = A2(
			_opensolid$geometry$OpenSolid_Point2d$vectorFrom,
			_opensolid$geometry$OpenSolid_Bootstrap_Axis2d$originPoint(axis),
			point);
		var directionVector = _opensolid$geometry$OpenSolid_Direction2d$toVector(
			_opensolid$geometry$OpenSolid_Bootstrap_Axis2d$direction(axis));
		return A2(_opensolid$geometry$OpenSolid_Vector2d$crossProduct, directionVector, displacementVector);
	});
var _opensolid$geometry$OpenSolid_Point2d$relativeTo = F2(
	function (frame, point) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
			_opensolid$geometry$OpenSolid_Vector2d$components(
				A2(
					_opensolid$geometry$OpenSolid_Vector2d$relativeTo,
					frame,
					A2(
						_opensolid$geometry$OpenSolid_Point2d$vectorFrom,
						_opensolid$geometry$OpenSolid_Bootstrap_Frame2d$originPoint(frame),
						point))));
	});
var _opensolid$geometry$OpenSolid_Point2d$translateBy = F2(
	function (vector, point) {
		var _p15 = _opensolid$geometry$OpenSolid_Point2d$coordinates(point);
		var px = _p15._0;
		var py = _p15._1;
		var _p16 = _opensolid$geometry$OpenSolid_Vector2d$components(vector);
		var vx = _p16._0;
		var vy = _p16._1;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
			{ctor: '_Tuple2', _0: px + vx, _1: py + vy});
	});
var _opensolid$geometry$OpenSolid_Point2d$hull = F2(
	function (firstPoint, secondPoint) {
		var _p17 = _opensolid$geometry$OpenSolid_Point2d$coordinates(secondPoint);
		var x2 = _p17._0;
		var y2 = _p17._1;
		var _p18 = _opensolid$geometry$OpenSolid_Point2d$coordinates(firstPoint);
		var x1 = _p18._0;
		var y1 = _p18._1;
		return _opensolid$geometry$OpenSolid_Geometry_Types$BoundingBox2d(
			{
				minX: A2(_elm_lang$core$Basics$min, x1, x2),
				maxX: A2(_elm_lang$core$Basics$max, x1, x2),
				minY: A2(_elm_lang$core$Basics$min, y1, y2),
				maxY: A2(_elm_lang$core$Basics$max, y1, y2)
			});
	});
var _opensolid$geometry$OpenSolid_Point2d$along = F2(
	function (axis, distance) {
		return A2(
			_opensolid$geometry$OpenSolid_Point2d$translateBy,
			A2(
				_opensolid$geometry$OpenSolid_Vector2d$in_,
				_opensolid$geometry$OpenSolid_Bootstrap_Axis2d$direction(axis),
				distance),
			_opensolid$geometry$OpenSolid_Bootstrap_Axis2d$originPoint(axis));
	});
var _opensolid$geometry$OpenSolid_Point2d$interpolateFrom = F3(
	function (p1, p2, t) {
		var _p19 = _opensolid$geometry$OpenSolid_Point2d$coordinates(p2);
		var x2 = _p19._0;
		var y2 = _p19._1;
		var _p20 = _opensolid$geometry$OpenSolid_Point2d$coordinates(p1);
		var x1 = _p20._0;
		var y1 = _p20._1;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
			{
				ctor: '_Tuple2',
				_0: A3(_opensolid$geometry$OpenSolid_Scalar$interpolateFrom, x1, x2, t),
				_1: A3(_opensolid$geometry$OpenSolid_Scalar$interpolateFrom, y1, y2, t)
			});
	});
var _opensolid$geometry$OpenSolid_Point2d$interpolate = _opensolid$geometry$OpenSolid_Point2d$interpolateFrom;
var _opensolid$geometry$OpenSolid_Point2d$midpoint = F2(
	function (firstPoint, secondPoint) {
		return A3(_opensolid$geometry$OpenSolid_Point2d$interpolateFrom, firstPoint, secondPoint, 0.5);
	});
var _opensolid$geometry$OpenSolid_Point2d$polar = function (coordinates) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
		_elm_lang$core$Basics$fromPolar(coordinates));
};
var _opensolid$geometry$OpenSolid_Point2d$origin = _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
	{ctor: '_Tuple2', _0: 0, _1: 0});
var _opensolid$geometry$OpenSolid_Point2d$addTo = F2(
	function (point, vector) {
		return A2(_opensolid$geometry$OpenSolid_Point2d$translateBy, vector, point);
	});
var _opensolid$geometry$OpenSolid_Point2d$scaleAbout = F3(
	function (centerPoint, scale, point) {
		return A2(
			_opensolid$geometry$OpenSolid_Point2d$addTo,
			centerPoint,
			A2(
				_opensolid$geometry$OpenSolid_Vector2d$scaleBy,
				scale,
				A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, centerPoint, point)));
	});
var _opensolid$geometry$OpenSolid_Point2d$rotateAround = F2(
	function (centerPoint, angle) {
		return function (_p21) {
			return A2(
				_opensolid$geometry$OpenSolid_Point2d$addTo,
				centerPoint,
				A2(
					_opensolid$geometry$OpenSolid_Vector2d$rotateBy,
					angle,
					A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, centerPoint, _p21)));
		};
	});
var _opensolid$geometry$OpenSolid_Point2d$mirrorAcross = function (axis) {
	return function (_p22) {
		return A2(
			_opensolid$geometry$OpenSolid_Point2d$addTo,
			_opensolid$geometry$OpenSolid_Bootstrap_Axis2d$originPoint(axis),
			A2(
				_opensolid$geometry$OpenSolid_Vector2d$mirrorAcross,
				axis,
				A2(
					_opensolid$geometry$OpenSolid_Point2d$vectorFrom,
					_opensolid$geometry$OpenSolid_Bootstrap_Axis2d$originPoint(axis),
					_p22)));
	};
};
var _opensolid$geometry$OpenSolid_Point2d$projectOnto = function (axis) {
	return function (_p23) {
		return A2(
			_opensolid$geometry$OpenSolid_Point2d$addTo,
			_opensolid$geometry$OpenSolid_Bootstrap_Axis2d$originPoint(axis),
			A2(
				_opensolid$geometry$OpenSolid_Vector2d$projectOnto,
				axis,
				A2(
					_opensolid$geometry$OpenSolid_Point2d$vectorFrom,
					_opensolid$geometry$OpenSolid_Bootstrap_Axis2d$originPoint(axis),
					_p23)));
	};
};
var _opensolid$geometry$OpenSolid_Point2d$placeIn = F2(
	function (frame, point) {
		return A2(
			_opensolid$geometry$OpenSolid_Point2d$addTo,
			_opensolid$geometry$OpenSolid_Bootstrap_Frame2d$originPoint(frame),
			A2(
				_opensolid$geometry$OpenSolid_Vector2d$placeIn,
				frame,
				_opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
					_opensolid$geometry$OpenSolid_Point2d$coordinates(point))));
	});
var _opensolid$geometry$OpenSolid_Point2d$in_ = F2(
	function (frame, coordinates) {
		return A2(
			_opensolid$geometry$OpenSolid_Point2d$placeIn,
			frame,
			_opensolid$geometry$OpenSolid_Geometry_Types$Point2d(coordinates));
	});

var _opensolid$geometry$OpenSolid_Axis2d$direction = function (_p0) {
	var _p1 = _p0;
	return _p1._0.direction;
};
var _opensolid$geometry$OpenSolid_Axis2d$moveTo = F2(
	function (newOrigin, axis) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d(
			{
				originPoint: newOrigin,
				direction: _opensolid$geometry$OpenSolid_Axis2d$direction(axis)
			});
	});
var _opensolid$geometry$OpenSolid_Axis2d$originPoint = function (_p2) {
	var _p3 = _p2;
	return _p3._0.originPoint;
};
var _opensolid$geometry$OpenSolid_Axis2d$flip = function (axis) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d(
		{
			originPoint: _opensolid$geometry$OpenSolid_Axis2d$originPoint(axis),
			direction: _opensolid$geometry$OpenSolid_Direction2d$flip(
				_opensolid$geometry$OpenSolid_Axis2d$direction(axis))
		});
};
var _opensolid$geometry$OpenSolid_Axis2d$rotateAround = F2(
	function (centerPoint, angle) {
		var rotateDirection = _opensolid$geometry$OpenSolid_Direction2d$rotateBy(angle);
		var rotatePoint = A2(_opensolid$geometry$OpenSolid_Point2d$rotateAround, centerPoint, angle);
		return function (axis) {
			return _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d(
				{
					originPoint: rotatePoint(
						_opensolid$geometry$OpenSolid_Axis2d$originPoint(axis)),
					direction: rotateDirection(
						_opensolid$geometry$OpenSolid_Axis2d$direction(axis))
				});
		};
	});
var _opensolid$geometry$OpenSolid_Axis2d$translateBy = F2(
	function (vector, axis) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d(
			{
				originPoint: A2(
					_opensolid$geometry$OpenSolid_Point2d$translateBy,
					vector,
					_opensolid$geometry$OpenSolid_Axis2d$originPoint(axis)),
				direction: _opensolid$geometry$OpenSolid_Axis2d$direction(axis)
			});
	});
var _opensolid$geometry$OpenSolid_Axis2d$mirrorAcross = function (otherAxis) {
	var mirrorDirection = _opensolid$geometry$OpenSolid_Direction2d$mirrorAcross(otherAxis);
	var mirrorPoint = _opensolid$geometry$OpenSolid_Point2d$mirrorAcross(otherAxis);
	return function (axis) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d(
			{
				originPoint: mirrorPoint(
					_opensolid$geometry$OpenSolid_Axis2d$originPoint(axis)),
				direction: mirrorDirection(
					_opensolid$geometry$OpenSolid_Axis2d$direction(axis))
			});
	};
};
var _opensolid$geometry$OpenSolid_Axis2d$relativeTo = function (frame) {
	var relativeDirection = _opensolid$geometry$OpenSolid_Direction2d$relativeTo(frame);
	var relativePoint = _opensolid$geometry$OpenSolid_Point2d$relativeTo(frame);
	return function (axis) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d(
			{
				originPoint: relativePoint(
					_opensolid$geometry$OpenSolid_Axis2d$originPoint(axis)),
				direction: relativeDirection(
					_opensolid$geometry$OpenSolid_Axis2d$direction(axis))
			});
	};
};
var _opensolid$geometry$OpenSolid_Axis2d$placeIn = function (frame) {
	var placeDirection = _opensolid$geometry$OpenSolid_Direction2d$placeIn(frame);
	var placePoint = _opensolid$geometry$OpenSolid_Point2d$placeIn(frame);
	return function (axis) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d(
			{
				originPoint: placePoint(
					_opensolid$geometry$OpenSolid_Axis2d$originPoint(axis)),
				direction: placeDirection(
					_opensolid$geometry$OpenSolid_Axis2d$direction(axis))
			});
	};
};
var _opensolid$geometry$OpenSolid_Axis2d$placeOnto = function (sketchPlane) {
	var placeDirection = _opensolid$geometry$OpenSolid_Direction2d$placeOnto(sketchPlane);
	var placePoint = _opensolid$geometry$OpenSolid_Point2d$placeOnto(sketchPlane);
	return function (axis) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
			{
				originPoint: placePoint(
					_opensolid$geometry$OpenSolid_Axis2d$originPoint(axis)),
				direction: placeDirection(
					_opensolid$geometry$OpenSolid_Axis2d$direction(axis))
			});
	};
};
var _opensolid$geometry$OpenSolid_Axis2d$y = _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d(
	{originPoint: _opensolid$geometry$OpenSolid_Point2d$origin, direction: _opensolid$geometry$OpenSolid_Direction2d$y});
var _opensolid$geometry$OpenSolid_Axis2d$x = _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d(
	{originPoint: _opensolid$geometry$OpenSolid_Point2d$origin, direction: _opensolid$geometry$OpenSolid_Direction2d$x});

var _opensolid$geometry$OpenSolid_Frame2d$yDirection = function (_p0) {
	var _p1 = _p0;
	return _p1._0.yDirection;
};
var _opensolid$geometry$OpenSolid_Frame2d$xDirection = function (_p2) {
	var _p3 = _p2;
	return _p3._0.xDirection;
};
var _opensolid$geometry$OpenSolid_Frame2d$isRightHanded = function (frame) {
	var yVector = _opensolid$geometry$OpenSolid_Direction2d$toVector(
		_opensolid$geometry$OpenSolid_Frame2d$yDirection(frame));
	var xVector = _opensolid$geometry$OpenSolid_Direction2d$toVector(
		_opensolid$geometry$OpenSolid_Frame2d$xDirection(frame));
	return _elm_lang$core$Native_Utils.cmp(
		A2(_opensolid$geometry$OpenSolid_Vector2d$crossProduct, xVector, yVector),
		0) > 0;
};
var _opensolid$geometry$OpenSolid_Frame2d$moveTo = F2(
	function (newOrigin, frame) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Frame2d(
			{
				originPoint: newOrigin,
				xDirection: _opensolid$geometry$OpenSolid_Frame2d$xDirection(frame),
				yDirection: _opensolid$geometry$OpenSolid_Frame2d$yDirection(frame)
			});
	});
var _opensolid$geometry$OpenSolid_Frame2d$originPoint = function (_p4) {
	var _p5 = _p4;
	return _p5._0.originPoint;
};
var _opensolid$geometry$OpenSolid_Frame2d$xAxis = function (frame) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d(
		{
			originPoint: _opensolid$geometry$OpenSolid_Frame2d$originPoint(frame),
			direction: _opensolid$geometry$OpenSolid_Frame2d$xDirection(frame)
		});
};
var _opensolid$geometry$OpenSolid_Frame2d$yAxis = function (frame) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d(
		{
			originPoint: _opensolid$geometry$OpenSolid_Frame2d$originPoint(frame),
			direction: _opensolid$geometry$OpenSolid_Frame2d$yDirection(frame)
		});
};
var _opensolid$geometry$OpenSolid_Frame2d$flipX = function (frame) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Frame2d(
		{
			originPoint: _opensolid$geometry$OpenSolid_Frame2d$originPoint(frame),
			xDirection: _opensolid$geometry$OpenSolid_Direction2d$flip(
				_opensolid$geometry$OpenSolid_Frame2d$xDirection(frame)),
			yDirection: _opensolid$geometry$OpenSolid_Frame2d$yDirection(frame)
		});
};
var _opensolid$geometry$OpenSolid_Frame2d$flipY = function (frame) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Frame2d(
		{
			originPoint: _opensolid$geometry$OpenSolid_Frame2d$originPoint(frame),
			xDirection: _opensolid$geometry$OpenSolid_Frame2d$xDirection(frame),
			yDirection: _opensolid$geometry$OpenSolid_Direction2d$flip(
				_opensolid$geometry$OpenSolid_Frame2d$yDirection(frame))
		});
};
var _opensolid$geometry$OpenSolid_Frame2d$rotateBy = F2(
	function (angle, frame) {
		var rotateDirection = _opensolid$geometry$OpenSolid_Direction2d$rotateBy(angle);
		return _opensolid$geometry$OpenSolid_Geometry_Types$Frame2d(
			{
				originPoint: _opensolid$geometry$OpenSolid_Frame2d$originPoint(frame),
				xDirection: rotateDirection(
					_opensolid$geometry$OpenSolid_Frame2d$xDirection(frame)),
				yDirection: rotateDirection(
					_opensolid$geometry$OpenSolid_Frame2d$yDirection(frame))
			});
	});
var _opensolid$geometry$OpenSolid_Frame2d$rotateAround = F2(
	function (centerPoint, angle) {
		var rotateDirection = _opensolid$geometry$OpenSolid_Direction2d$rotateBy(angle);
		var rotatePoint = A2(_opensolid$geometry$OpenSolid_Point2d$rotateAround, centerPoint, angle);
		return function (frame) {
			return _opensolid$geometry$OpenSolid_Geometry_Types$Frame2d(
				{
					originPoint: rotatePoint(
						_opensolid$geometry$OpenSolid_Frame2d$originPoint(frame)),
					xDirection: rotateDirection(
						_opensolid$geometry$OpenSolid_Frame2d$xDirection(frame)),
					yDirection: rotateDirection(
						_opensolid$geometry$OpenSolid_Frame2d$yDirection(frame))
				});
		};
	});
var _opensolid$geometry$OpenSolid_Frame2d$translateBy = F2(
	function (vector, frame) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Frame2d(
			{
				originPoint: A2(
					_opensolid$geometry$OpenSolid_Point2d$translateBy,
					vector,
					_opensolid$geometry$OpenSolid_Frame2d$originPoint(frame)),
				xDirection: _opensolid$geometry$OpenSolid_Frame2d$xDirection(frame),
				yDirection: _opensolid$geometry$OpenSolid_Frame2d$yDirection(frame)
			});
	});
var _opensolid$geometry$OpenSolid_Frame2d$translateAlongOwn = F3(
	function (axis, distance, frame) {
		var direction = _opensolid$geometry$OpenSolid_Axis2d$direction(
			axis(frame));
		return A2(
			_opensolid$geometry$OpenSolid_Frame2d$translateBy,
			A2(_opensolid$geometry$OpenSolid_Vector2d$in_, direction, distance),
			frame);
	});
var _opensolid$geometry$OpenSolid_Frame2d$mirrorAcross = function (axis) {
	var mirrorDirection = _opensolid$geometry$OpenSolid_Direction2d$mirrorAcross(axis);
	var mirrorPoint = _opensolid$geometry$OpenSolid_Point2d$mirrorAcross(axis);
	return function (frame) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Frame2d(
			{
				originPoint: mirrorPoint(
					_opensolid$geometry$OpenSolid_Frame2d$originPoint(frame)),
				xDirection: mirrorDirection(
					_opensolid$geometry$OpenSolid_Frame2d$xDirection(frame)),
				yDirection: mirrorDirection(
					_opensolid$geometry$OpenSolid_Frame2d$yDirection(frame))
			});
	};
};
var _opensolid$geometry$OpenSolid_Frame2d$relativeTo = function (otherFrame) {
	var relativeDirection = _opensolid$geometry$OpenSolid_Direction2d$relativeTo(otherFrame);
	var relativePoint = _opensolid$geometry$OpenSolid_Point2d$relativeTo(otherFrame);
	return function (frame) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Frame2d(
			{
				originPoint: relativePoint(
					_opensolid$geometry$OpenSolid_Frame2d$originPoint(frame)),
				xDirection: relativeDirection(
					_opensolid$geometry$OpenSolid_Frame2d$xDirection(frame)),
				yDirection: relativeDirection(
					_opensolid$geometry$OpenSolid_Frame2d$yDirection(frame))
			});
	};
};
var _opensolid$geometry$OpenSolid_Frame2d$placeIn = function (otherFrame) {
	var placeDirection = _opensolid$geometry$OpenSolid_Direction2d$placeIn(otherFrame);
	var placePoint = _opensolid$geometry$OpenSolid_Point2d$placeIn(otherFrame);
	return function (frame) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Frame2d(
			{
				originPoint: placePoint(
					_opensolid$geometry$OpenSolid_Frame2d$originPoint(frame)),
				xDirection: placeDirection(
					_opensolid$geometry$OpenSolid_Frame2d$xDirection(frame)),
				yDirection: placeDirection(
					_opensolid$geometry$OpenSolid_Frame2d$yDirection(frame))
			});
	};
};
var _opensolid$geometry$OpenSolid_Frame2d$at = function (point) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Frame2d(
		{originPoint: point, xDirection: _opensolid$geometry$OpenSolid_Direction2d$x, yDirection: _opensolid$geometry$OpenSolid_Direction2d$y});
};
var _opensolid$geometry$OpenSolid_Frame2d$xy = _opensolid$geometry$OpenSolid_Frame2d$at(_opensolid$geometry$OpenSolid_Point2d$origin);

var _opensolid$geometry$OpenSolid_LineSegment2d$endpoints = function (_p0) {
	var _p1 = _p0;
	return _p1._0;
};
var _opensolid$geometry$OpenSolid_LineSegment2d$reverse = function (lineSegment) {
	var _p2 = _opensolid$geometry$OpenSolid_LineSegment2d$endpoints(lineSegment);
	var p1 = _p2._0;
	var p2 = _p2._1;
	return _opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d(
		{ctor: '_Tuple2', _0: p2, _1: p1});
};
var _opensolid$geometry$OpenSolid_LineSegment2d$interpolate = function (lineSegment) {
	var _p3 = _opensolid$geometry$OpenSolid_LineSegment2d$endpoints(lineSegment);
	var start = _p3._0;
	var end = _p3._1;
	return A2(_opensolid$geometry$OpenSolid_Point2d$interpolateFrom, start, end);
};
var _opensolid$geometry$OpenSolid_LineSegment2d$midpoint = function (lineSegment) {
	return A2(_opensolid$geometry$OpenSolid_LineSegment2d$interpolate, lineSegment, 0.5);
};
var _opensolid$geometry$OpenSolid_LineSegment2d$vector = function (lineSegment) {
	var _p4 = _opensolid$geometry$OpenSolid_LineSegment2d$endpoints(lineSegment);
	var p1 = _p4._0;
	var p2 = _p4._1;
	return A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p1, p2);
};
var _opensolid$geometry$OpenSolid_LineSegment2d$length = function (_p5) {
	return _opensolid$geometry$OpenSolid_Vector2d$length(
		_opensolid$geometry$OpenSolid_LineSegment2d$vector(_p5));
};
var _opensolid$geometry$OpenSolid_LineSegment2d$squaredLength = function (_p6) {
	return _opensolid$geometry$OpenSolid_Vector2d$squaredLength(
		_opensolid$geometry$OpenSolid_LineSegment2d$vector(_p6));
};
var _opensolid$geometry$OpenSolid_LineSegment2d$direction = function (_p7) {
	return _opensolid$geometry$OpenSolid_Vector2d$direction(
		_opensolid$geometry$OpenSolid_LineSegment2d$vector(_p7));
};
var _opensolid$geometry$OpenSolid_LineSegment2d$normalDirection = function (_p8) {
	return _opensolid$geometry$OpenSolid_Vector2d$direction(
		_opensolid$geometry$OpenSolid_Vector2d$perpendicularTo(
			_opensolid$geometry$OpenSolid_LineSegment2d$vector(_p8)));
};
var _opensolid$geometry$OpenSolid_LineSegment2d$intersectionPoint = F2(
	function (lineSegment1, lineSegment2) {
		var _p9 = _opensolid$geometry$OpenSolid_LineSegment2d$endpoints(lineSegment2);
		var q = _p9._0;
		var q_ = _p9._1;
		var _p10 = _opensolid$geometry$OpenSolid_LineSegment2d$endpoints(lineSegment1);
		var p = _p10._0;
		var p_ = _p10._1;
		var _p11 = {
			ctor: '_Tuple5',
			_0: _opensolid$geometry$OpenSolid_LineSegment2d$vector(lineSegment1),
			_1: _opensolid$geometry$OpenSolid_LineSegment2d$vector(lineSegment2),
			_2: A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p, q),
			_3: A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p, q_),
			_4: A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, q, p_)
		};
		var r = _p11._0;
		var s = _p11._1;
		var pq = _p11._2;
		var pq_ = _p11._3;
		var qp_ = _p11._4;
		var _p12 = {
			ctor: '_Tuple4',
			_0: A2(_opensolid$geometry$OpenSolid_Vector2d$crossProduct, pq, r),
			_1: A2(_opensolid$geometry$OpenSolid_Vector2d$crossProduct, pq, s),
			_2: A2(_opensolid$geometry$OpenSolid_Vector2d$crossProduct, s, qp_),
			_3: A2(_opensolid$geometry$OpenSolid_Vector2d$crossProduct, r, pq_)
		};
		var pqXr = _p12._0;
		var pqXs = _p12._1;
		var sXqp_ = _p12._2;
		var rXpq_ = _p12._3;
		var _p13 = {ctor: '_Tuple2', _0: pqXs - sXqp_, _1: pqXr + rXpq_};
		var tDenominator = _p13._0;
		var uDenominator = _p13._1;
		if (_elm_lang$core$Native_Utils.eq(tDenominator, 0) || _elm_lang$core$Native_Utils.eq(uDenominator, 0)) {
			return (_elm_lang$core$Native_Utils.cmp(
				A2(_opensolid$geometry$OpenSolid_Vector2d$dotProduct, r, s),
				0) < 0) ? (_elm_lang$core$Native_Utils.eq(p_, q_) ? _elm_lang$core$Maybe$Just(p_) : (_elm_lang$core$Native_Utils.eq(p, q) ? _elm_lang$core$Maybe$Just(p) : _elm_lang$core$Maybe$Nothing)) : (_elm_lang$core$Native_Utils.eq(p_, q) ? _elm_lang$core$Maybe$Just(p_) : (_elm_lang$core$Native_Utils.eq(p, q_) ? _elm_lang$core$Maybe$Just(p) : _elm_lang$core$Maybe$Nothing));
		} else {
			var _p14 = {ctor: '_Tuple2', _0: pqXs / tDenominator, _1: pqXr / uDenominator};
			var t = _p14._0;
			var u = _p14._1;
			if (((_elm_lang$core$Native_Utils.cmp(0, t) < 1) && (_elm_lang$core$Native_Utils.cmp(t, 1) < 1)) && ((_elm_lang$core$Native_Utils.cmp(0, u) < 1) && (_elm_lang$core$Native_Utils.cmp(u, 1) < 1))) {
				var intersection = (_elm_lang$core$Native_Utils.cmp(
					A2(_elm_lang$core$Basics$min, t, 1 - t),
					A2(_elm_lang$core$Basics$min, u, 1 - u)) < 1) ? A2(_opensolid$geometry$OpenSolid_LineSegment2d$interpolate, lineSegment1, t) : A2(_opensolid$geometry$OpenSolid_LineSegment2d$interpolate, lineSegment2, u);
				return _elm_lang$core$Maybe$Just(intersection);
			} else {
				return _elm_lang$core$Maybe$Nothing;
			}
		}
	});
var _opensolid$geometry$OpenSolid_LineSegment2d$map = F2(
	function ($function, lineSegment) {
		var _p15 = _opensolid$geometry$OpenSolid_LineSegment2d$endpoints(lineSegment);
		var p1 = _p15._0;
		var p2 = _p15._1;
		return _opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d(
			{
				ctor: '_Tuple2',
				_0: $function(p1),
				_1: $function(p2)
			});
	});
var _opensolid$geometry$OpenSolid_LineSegment2d$scaleAbout = F2(
	function (point, scale) {
		return _opensolid$geometry$OpenSolid_LineSegment2d$map(
			A2(_opensolid$geometry$OpenSolid_Point2d$scaleAbout, point, scale));
	});
var _opensolid$geometry$OpenSolid_LineSegment2d$rotateAround = F2(
	function (centerPoint, angle) {
		return _opensolid$geometry$OpenSolid_LineSegment2d$map(
			A2(_opensolid$geometry$OpenSolid_Point2d$rotateAround, centerPoint, angle));
	});
var _opensolid$geometry$OpenSolid_LineSegment2d$translateBy = function (vector) {
	return _opensolid$geometry$OpenSolid_LineSegment2d$map(
		_opensolid$geometry$OpenSolid_Point2d$translateBy(vector));
};
var _opensolid$geometry$OpenSolid_LineSegment2d$mirrorAcross = function (axis) {
	return _opensolid$geometry$OpenSolid_LineSegment2d$map(
		_opensolid$geometry$OpenSolid_Point2d$mirrorAcross(axis));
};
var _opensolid$geometry$OpenSolid_LineSegment2d$projectOnto = function (axis) {
	return _opensolid$geometry$OpenSolid_LineSegment2d$map(
		_opensolid$geometry$OpenSolid_Point2d$projectOnto(axis));
};
var _opensolid$geometry$OpenSolid_LineSegment2d$relativeTo = function (frame) {
	return _opensolid$geometry$OpenSolid_LineSegment2d$map(
		_opensolid$geometry$OpenSolid_Point2d$relativeTo(frame));
};
var _opensolid$geometry$OpenSolid_LineSegment2d$placeIn = function (frame) {
	return _opensolid$geometry$OpenSolid_LineSegment2d$map(
		_opensolid$geometry$OpenSolid_Point2d$placeIn(frame));
};
var _opensolid$geometry$OpenSolid_LineSegment2d$placeOnto = function (sketchPlane) {
	var place = _opensolid$geometry$OpenSolid_Point2d$placeOnto(sketchPlane);
	return function (lineSegment) {
		var _p16 = _opensolid$geometry$OpenSolid_LineSegment2d$endpoints(lineSegment);
		var p1 = _p16._0;
		var p2 = _p16._1;
		return _opensolid$geometry$OpenSolid_Geometry_Types$LineSegment3d(
			{
				ctor: '_Tuple2',
				_0: place(p1),
				_1: place(p2)
			});
	};
};
var _opensolid$geometry$OpenSolid_LineSegment2d$boundingBox = function (lineSegment) {
	var _p17 = _opensolid$geometry$OpenSolid_LineSegment2d$endpoints(lineSegment);
	var p1 = _p17._0;
	var p2 = _p17._1;
	return A2(_opensolid$geometry$OpenSolid_Point2d$hull, p1, p2);
};
var _opensolid$geometry$OpenSolid_LineSegment2d$endPoint = function (_p18) {
	var _p19 = _p18;
	return _p19._0._1;
};
var _opensolid$geometry$OpenSolid_LineSegment2d$startPoint = function (_p20) {
	var _p21 = _p20;
	return _p21._0._0;
};
var _opensolid$geometry$OpenSolid_LineSegment2d$along = F3(
	function (axis, start, end) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d(
			{
				ctor: '_Tuple2',
				_0: A2(_opensolid$geometry$OpenSolid_Point2d$along, axis, start),
				_1: A2(_opensolid$geometry$OpenSolid_Point2d$along, axis, end)
			});
	});

var _opensolid$geometry$OpenSolid_Triangle2d$vertices = function (_p0) {
	var _p1 = _p0;
	return _p1._0;
};
var _opensolid$geometry$OpenSolid_Triangle2d$edges = function (triangle) {
	var _p2 = _opensolid$geometry$OpenSolid_Triangle2d$vertices(triangle);
	var p1 = _p2._0;
	var p2 = _p2._1;
	var p3 = _p2._2;
	return {
		ctor: '_Tuple3',
		_0: _opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d(
			{ctor: '_Tuple2', _0: p1, _1: p2}),
		_1: _opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d(
			{ctor: '_Tuple2', _0: p2, _1: p3}),
		_2: _opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d(
			{ctor: '_Tuple2', _0: p3, _1: p1})
	};
};
var _opensolid$geometry$OpenSolid_Triangle2d$centroid = function (triangle) {
	var _p3 = _opensolid$geometry$OpenSolid_Triangle2d$vertices(triangle);
	var p1 = _p3._0;
	var p2 = _p3._1;
	var p3 = _p3._2;
	var firstVector = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p1, p2);
	var secondVector = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p1, p3);
	var displacement = A2(
		_opensolid$geometry$OpenSolid_Vector2d$scaleBy,
		1.0 / 3.0,
		A2(_opensolid$geometry$OpenSolid_Vector2d$sum, firstVector, secondVector));
	return A2(_opensolid$geometry$OpenSolid_Point2d$translateBy, displacement, p1);
};
var _opensolid$geometry$OpenSolid_Triangle2d$contains = F2(
	function (point, triangle) {
		var crossProduct = F2(
			function (startVertex, endVertex) {
				var segmentVector = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, startVertex, endVertex);
				var vectorToPoint = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, startVertex, point);
				return A2(_opensolid$geometry$OpenSolid_Vector2d$crossProduct, segmentVector, vectorToPoint);
			});
		var _p4 = _opensolid$geometry$OpenSolid_Triangle2d$vertices(triangle);
		var p1 = _p4._0;
		var p2 = _p4._1;
		var p3 = _p4._2;
		var firstProduct = A2(crossProduct, p1, p2);
		var secondProduct = A2(crossProduct, p2, p3);
		var thirdProduct = A2(crossProduct, p3, p1);
		return ((_elm_lang$core$Native_Utils.cmp(firstProduct, 0) > -1) && ((_elm_lang$core$Native_Utils.cmp(secondProduct, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(thirdProduct, 0) > -1))) || ((_elm_lang$core$Native_Utils.cmp(firstProduct, 0) < 1) && ((_elm_lang$core$Native_Utils.cmp(secondProduct, 0) < 1) && (_elm_lang$core$Native_Utils.cmp(thirdProduct, 0) < 1)));
	});
var _opensolid$geometry$OpenSolid_Triangle2d$counterclockwiseArea = function (triangle) {
	var _p5 = _opensolid$geometry$OpenSolid_Triangle2d$vertices(triangle);
	var p1 = _p5._0;
	var p2 = _p5._1;
	var p3 = _p5._2;
	var firstVector = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p1, p2);
	var secondVector = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p1, p3);
	return 0.5 * A2(_opensolid$geometry$OpenSolid_Vector2d$crossProduct, firstVector, secondVector);
};
var _opensolid$geometry$OpenSolid_Triangle2d$area = function (_p6) {
	return _elm_lang$core$Basics$abs(
		_opensolid$geometry$OpenSolid_Triangle2d$counterclockwiseArea(_p6));
};
var _opensolid$geometry$OpenSolid_Triangle2d$clockwiseArea = function (triangle) {
	return 0 - _opensolid$geometry$OpenSolid_Triangle2d$counterclockwiseArea(triangle);
};
var _opensolid$geometry$OpenSolid_Triangle2d$map = F2(
	function ($function, triangle) {
		var _p7 = _opensolid$geometry$OpenSolid_Triangle2d$vertices(triangle);
		var p1 = _p7._0;
		var p2 = _p7._1;
		var p3 = _p7._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Triangle2d(
			{
				ctor: '_Tuple3',
				_0: $function(p1),
				_1: $function(p2),
				_2: $function(p3)
			});
	});
var _opensolid$geometry$OpenSolid_Triangle2d$scaleAbout = F2(
	function (point, scale) {
		return _opensolid$geometry$OpenSolid_Triangle2d$map(
			A2(_opensolid$geometry$OpenSolid_Point2d$scaleAbout, point, scale));
	});
var _opensolid$geometry$OpenSolid_Triangle2d$rotateAround = F2(
	function (centerPoint, angle) {
		return _opensolid$geometry$OpenSolid_Triangle2d$map(
			A2(_opensolid$geometry$OpenSolid_Point2d$rotateAround, centerPoint, angle));
	});
var _opensolid$geometry$OpenSolid_Triangle2d$translateBy = function (vector) {
	return _opensolid$geometry$OpenSolid_Triangle2d$map(
		_opensolid$geometry$OpenSolid_Point2d$translateBy(vector));
};
var _opensolid$geometry$OpenSolid_Triangle2d$mirrorAcross = function (axis) {
	return _opensolid$geometry$OpenSolid_Triangle2d$map(
		_opensolid$geometry$OpenSolid_Point2d$mirrorAcross(axis));
};
var _opensolid$geometry$OpenSolid_Triangle2d$relativeTo = function (frame) {
	return _opensolid$geometry$OpenSolid_Triangle2d$map(
		_opensolid$geometry$OpenSolid_Point2d$relativeTo(frame));
};
var _opensolid$geometry$OpenSolid_Triangle2d$placeIn = function (frame) {
	return _opensolid$geometry$OpenSolid_Triangle2d$map(
		_opensolid$geometry$OpenSolid_Point2d$placeIn(frame));
};
var _opensolid$geometry$OpenSolid_Triangle2d$placeOnto = function (sketchPlane) {
	var place = _opensolid$geometry$OpenSolid_Point2d$placeOnto(sketchPlane);
	return function (triangle) {
		var _p8 = _opensolid$geometry$OpenSolid_Triangle2d$vertices(triangle);
		var p1 = _p8._0;
		var p2 = _p8._1;
		var p3 = _p8._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Triangle3d(
			{
				ctor: '_Tuple3',
				_0: place(p1),
				_1: place(p2),
				_2: place(p3)
			});
	};
};
var _opensolid$geometry$OpenSolid_Triangle2d$boundingBox = function (triangle) {
	var _p9 = _opensolid$geometry$OpenSolid_Triangle2d$vertices(triangle);
	var p1 = _p9._0;
	var p2 = _p9._1;
	var p3 = _p9._2;
	var _p10 = _opensolid$geometry$OpenSolid_Point2d$coordinates(p1);
	var x1 = _p10._0;
	var y1 = _p10._1;
	var _p11 = _opensolid$geometry$OpenSolid_Point2d$coordinates(p2);
	var x2 = _p11._0;
	var y2 = _p11._1;
	var _p12 = _opensolid$geometry$OpenSolid_Point2d$coordinates(p3);
	var x3 = _p12._0;
	var y3 = _p12._1;
	return _opensolid$geometry$OpenSolid_Geometry_Types$BoundingBox2d(
		{
			minX: A2(
				_elm_lang$core$Basics$min,
				x1,
				A2(_elm_lang$core$Basics$min, x2, x3)),
			maxX: A2(
				_elm_lang$core$Basics$max,
				x1,
				A2(_elm_lang$core$Basics$max, x2, x3)),
			minY: A2(
				_elm_lang$core$Basics$min,
				y1,
				A2(_elm_lang$core$Basics$min, y2, y3)),
			maxY: A2(
				_elm_lang$core$Basics$max,
				y1,
				A2(_elm_lang$core$Basics$max, y2, y3))
		});
};

var _opensolid$geometry$OpenSolid_BoundingBox2d$maxY = function (_p0) {
	var _p1 = _p0;
	return _p1._0.maxY;
};
var _opensolid$geometry$OpenSolid_BoundingBox2d$minY = function (_p2) {
	var _p3 = _p2;
	return _p3._0.minY;
};
var _opensolid$geometry$OpenSolid_BoundingBox2d$maxX = function (_p4) {
	var _p5 = _p4;
	return _p5._0.maxX;
};
var _opensolid$geometry$OpenSolid_BoundingBox2d$minX = function (_p6) {
	var _p7 = _p6;
	return _p7._0.minX;
};
var _opensolid$geometry$OpenSolid_BoundingBox2d$overlaps = F2(
	function (other, boundingBox) {
		return (_elm_lang$core$Native_Utils.cmp(
			_opensolid$geometry$OpenSolid_BoundingBox2d$minX(boundingBox),
			_opensolid$geometry$OpenSolid_BoundingBox2d$maxX(other)) < 1) && ((_elm_lang$core$Native_Utils.cmp(
			_opensolid$geometry$OpenSolid_BoundingBox2d$maxX(boundingBox),
			_opensolid$geometry$OpenSolid_BoundingBox2d$minX(other)) > -1) && ((_elm_lang$core$Native_Utils.cmp(
			_opensolid$geometry$OpenSolid_BoundingBox2d$minY(boundingBox),
			_opensolid$geometry$OpenSolid_BoundingBox2d$maxY(other)) < 1) && (_elm_lang$core$Native_Utils.cmp(
			_opensolid$geometry$OpenSolid_BoundingBox2d$maxY(boundingBox),
			_opensolid$geometry$OpenSolid_BoundingBox2d$minY(other)) > -1)));
	});
var _opensolid$geometry$OpenSolid_BoundingBox2d$isContainedIn = F2(
	function (other, boundingBox) {
		return ((_elm_lang$core$Native_Utils.cmp(
			_opensolid$geometry$OpenSolid_BoundingBox2d$minX(other),
			_opensolid$geometry$OpenSolid_BoundingBox2d$minX(boundingBox)) < 1) && (_elm_lang$core$Native_Utils.cmp(
			_opensolid$geometry$OpenSolid_BoundingBox2d$maxX(boundingBox),
			_opensolid$geometry$OpenSolid_BoundingBox2d$maxX(other)) < 1)) && ((_elm_lang$core$Native_Utils.cmp(
			_opensolid$geometry$OpenSolid_BoundingBox2d$minY(other),
			_opensolid$geometry$OpenSolid_BoundingBox2d$minY(boundingBox)) < 1) && (_elm_lang$core$Native_Utils.cmp(
			_opensolid$geometry$OpenSolid_BoundingBox2d$maxY(boundingBox),
			_opensolid$geometry$OpenSolid_BoundingBox2d$maxY(other)) < 1));
	});
var _opensolid$geometry$OpenSolid_BoundingBox2d$hull = F2(
	function (firstBox, secondBox) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$BoundingBox2d(
			{
				minX: A2(
					_elm_lang$core$Basics$min,
					_opensolid$geometry$OpenSolid_BoundingBox2d$minX(firstBox),
					_opensolid$geometry$OpenSolid_BoundingBox2d$minX(secondBox)),
				maxX: A2(
					_elm_lang$core$Basics$max,
					_opensolid$geometry$OpenSolid_BoundingBox2d$maxX(firstBox),
					_opensolid$geometry$OpenSolid_BoundingBox2d$maxX(secondBox)),
				minY: A2(
					_elm_lang$core$Basics$min,
					_opensolid$geometry$OpenSolid_BoundingBox2d$minY(firstBox),
					_opensolid$geometry$OpenSolid_BoundingBox2d$minY(secondBox)),
				maxY: A2(
					_elm_lang$core$Basics$max,
					_opensolid$geometry$OpenSolid_BoundingBox2d$maxY(firstBox),
					_opensolid$geometry$OpenSolid_BoundingBox2d$maxY(secondBox))
			});
	});
var _opensolid$geometry$OpenSolid_BoundingBox2d$intersection = F2(
	function (firstBox, secondBox) {
		return A2(_opensolid$geometry$OpenSolid_BoundingBox2d$overlaps, firstBox, secondBox) ? _elm_lang$core$Maybe$Just(
			_opensolid$geometry$OpenSolid_Geometry_Types$BoundingBox2d(
				{
					minX: A2(
						_elm_lang$core$Basics$max,
						_opensolid$geometry$OpenSolid_BoundingBox2d$minX(firstBox),
						_opensolid$geometry$OpenSolid_BoundingBox2d$minX(secondBox)),
					maxX: A2(
						_elm_lang$core$Basics$min,
						_opensolid$geometry$OpenSolid_BoundingBox2d$maxX(firstBox),
						_opensolid$geometry$OpenSolid_BoundingBox2d$maxX(secondBox)),
					minY: A2(
						_elm_lang$core$Basics$max,
						_opensolid$geometry$OpenSolid_BoundingBox2d$minY(firstBox),
						_opensolid$geometry$OpenSolid_BoundingBox2d$minY(secondBox)),
					maxY: A2(
						_elm_lang$core$Basics$min,
						_opensolid$geometry$OpenSolid_BoundingBox2d$maxY(firstBox),
						_opensolid$geometry$OpenSolid_BoundingBox2d$maxY(secondBox))
				})) : _elm_lang$core$Maybe$Nothing;
	});
var _opensolid$geometry$OpenSolid_BoundingBox2d$extrema = function (_p8) {
	var _p9 = _p8;
	return _p9._0;
};
var _opensolid$geometry$OpenSolid_BoundingBox2d$dimensions = function (boundingBox) {
	var _p10 = _opensolid$geometry$OpenSolid_BoundingBox2d$extrema(boundingBox);
	var minX = _p10.minX;
	var maxX = _p10.maxX;
	var minY = _p10.minY;
	var maxY = _p10.maxY;
	return {ctor: '_Tuple2', _0: maxX - minX, _1: maxY - minY};
};
var _opensolid$geometry$OpenSolid_BoundingBox2d$midX = function (boundingBox) {
	var _p11 = _opensolid$geometry$OpenSolid_BoundingBox2d$extrema(boundingBox);
	var minX = _p11.minX;
	var maxX = _p11.maxX;
	return minX + (0.5 * (maxX - minX));
};
var _opensolid$geometry$OpenSolid_BoundingBox2d$midY = function (boundingBox) {
	var _p12 = _opensolid$geometry$OpenSolid_BoundingBox2d$extrema(boundingBox);
	var minY = _p12.minY;
	var maxY = _p12.maxY;
	return minY + (0.5 * (maxY - minY));
};
var _opensolid$geometry$OpenSolid_BoundingBox2d$centroid = function (boundingBox) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
		{
			ctor: '_Tuple2',
			_0: _opensolid$geometry$OpenSolid_BoundingBox2d$midX(boundingBox),
			_1: _opensolid$geometry$OpenSolid_BoundingBox2d$midY(boundingBox)
		});
};
var _opensolid$geometry$OpenSolid_BoundingBox2d$contains = F2(
	function (point, boundingBox) {
		var _p13 = _opensolid$geometry$OpenSolid_BoundingBox2d$extrema(boundingBox);
		var minX = _p13.minX;
		var maxX = _p13.maxX;
		var minY = _p13.minY;
		var maxY = _p13.maxY;
		var _p14 = _opensolid$geometry$OpenSolid_Point2d$coordinates(point);
		var x = _p14._0;
		var y = _p14._1;
		return ((_elm_lang$core$Native_Utils.cmp(minX, x) < 1) && (_elm_lang$core$Native_Utils.cmp(x, maxX) < 1)) && ((_elm_lang$core$Native_Utils.cmp(minY, y) < 1) && (_elm_lang$core$Native_Utils.cmp(y, maxY) < 1));
	});
var _opensolid$geometry$OpenSolid_BoundingBox2d$hullOf = function (boundingBoxes) {
	var _p15 = boundingBoxes;
	if (_p15.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _opensolid$geometry$OpenSolid_BoundingBox2d$hull, _p15._0, _p15._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _opensolid$geometry$OpenSolid_BoundingBox2d$singleton = function (point) {
	var _p16 = _opensolid$geometry$OpenSolid_Point2d$coordinates(point);
	var x = _p16._0;
	var y = _p16._1;
	return _opensolid$geometry$OpenSolid_Geometry_Types$BoundingBox2d(
		{minX: x, maxX: x, minY: y, maxY: y});
};
var _opensolid$geometry$OpenSolid_BoundingBox2d$containing = function (points) {
	return _opensolid$geometry$OpenSolid_BoundingBox2d$hullOf(
		A2(_elm_lang$core$List$map, _opensolid$geometry$OpenSolid_BoundingBox2d$singleton, points));
};

var _opensolid$geometry$OpenSolid_Polyline2d$vertices = function (_p0) {
	var _p1 = _p0;
	return _p1._0;
};
var _opensolid$geometry$OpenSolid_Polyline2d$segments = function (polyline) {
	var _p2 = _opensolid$geometry$OpenSolid_Polyline2d$vertices(polyline);
	if (_p2.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		return A3(
			_elm_lang$core$List$map2,
			F2(
				function (start, end) {
					return _opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d(
						{ctor: '_Tuple2', _0: start, _1: end});
				}),
			_p2,
			_p2._1);
	}
};
var _opensolid$geometry$OpenSolid_Polyline2d$length = function (_p3) {
	return _elm_lang$core$List$sum(
		A2(
			_elm_lang$core$List$map,
			_opensolid$geometry$OpenSolid_LineSegment2d$length,
			_opensolid$geometry$OpenSolid_Polyline2d$segments(_p3)));
};
var _opensolid$geometry$OpenSolid_Polyline2d$map = function ($function) {
	return function (_p4) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Polyline2d(
			A2(
				_elm_lang$core$List$map,
				$function,
				_opensolid$geometry$OpenSolid_Polyline2d$vertices(_p4)));
	};
};
var _opensolid$geometry$OpenSolid_Polyline2d$scaleAbout = F2(
	function (point, scale) {
		return _opensolid$geometry$OpenSolid_Polyline2d$map(
			A2(_opensolid$geometry$OpenSolid_Point2d$scaleAbout, point, scale));
	});
var _opensolid$geometry$OpenSolid_Polyline2d$rotateAround = F2(
	function (point, angle) {
		return _opensolid$geometry$OpenSolid_Polyline2d$map(
			A2(_opensolid$geometry$OpenSolid_Point2d$rotateAround, point, angle));
	});
var _opensolid$geometry$OpenSolid_Polyline2d$translateBy = function (vector) {
	return _opensolid$geometry$OpenSolid_Polyline2d$map(
		_opensolid$geometry$OpenSolid_Point2d$translateBy(vector));
};
var _opensolid$geometry$OpenSolid_Polyline2d$mirrorAcross = function (axis) {
	return _opensolid$geometry$OpenSolid_Polyline2d$map(
		_opensolid$geometry$OpenSolid_Point2d$mirrorAcross(axis));
};
var _opensolid$geometry$OpenSolid_Polyline2d$projectOnto = function (axis) {
	return _opensolid$geometry$OpenSolid_Polyline2d$map(
		_opensolid$geometry$OpenSolid_Point2d$projectOnto(axis));
};
var _opensolid$geometry$OpenSolid_Polyline2d$relativeTo = function (frame) {
	return _opensolid$geometry$OpenSolid_Polyline2d$map(
		_opensolid$geometry$OpenSolid_Point2d$relativeTo(frame));
};
var _opensolid$geometry$OpenSolid_Polyline2d$placeIn = function (frame) {
	return _opensolid$geometry$OpenSolid_Polyline2d$map(
		_opensolid$geometry$OpenSolid_Point2d$placeIn(frame));
};
var _opensolid$geometry$OpenSolid_Polyline2d$placeOnto = function (sketchPlane) {
	return function (_p5) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Polyline3d(
			A2(
				_elm_lang$core$List$map,
				_opensolid$geometry$OpenSolid_Point2d$placeOnto(sketchPlane),
				_opensolid$geometry$OpenSolid_Polyline2d$vertices(_p5)));
	};
};
var _opensolid$geometry$OpenSolid_Polyline2d$boundingBox = function (polyline) {
	return _opensolid$geometry$OpenSolid_BoundingBox2d$containing(
		_opensolid$geometry$OpenSolid_Polyline2d$vertices(polyline));
};

var _opensolid$geometry$OpenSolid_Polygon2d$vertices = function (_p0) {
	var _p1 = _p0;
	return _p1._0;
};
var _opensolid$geometry$OpenSolid_Polygon2d$edges = function (polygon) {
	var _p2 = _opensolid$geometry$OpenSolid_Polygon2d$vertices(polygon);
	if (_p2.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		return A3(
			_elm_lang$core$List$map2,
			F2(
				function (start, end) {
					return _opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d(
						{ctor: '_Tuple2', _0: start, _1: end});
				}),
			_p2,
			A2(
				_elm_lang$core$Basics_ops['++'],
				_p2._1,
				{
					ctor: '::',
					_0: _p2._0,
					_1: {ctor: '[]'}
				}));
	}
};
var _opensolid$geometry$OpenSolid_Polygon2d$perimeter = function (_p3) {
	return _elm_lang$core$List$sum(
		A2(
			_elm_lang$core$List$map,
			_opensolid$geometry$OpenSolid_LineSegment2d$length,
			_opensolid$geometry$OpenSolid_Polygon2d$edges(_p3)));
};
var _opensolid$geometry$OpenSolid_Polygon2d$counterclockwiseArea = function (polygon) {
	var _p4 = _opensolid$geometry$OpenSolid_Polygon2d$vertices(polygon);
	if (_p4.ctor === '[]') {
		return 0;
	} else {
		if (_p4._1.ctor === '[]') {
			return 0;
		} else {
			if (_p4._1._1.ctor === '[]') {
				return 0;
			} else {
				var _p5 = _p4._1._1;
				var segmentArea = F2(
					function (start, end) {
						return _opensolid$geometry$OpenSolid_Triangle2d$counterclockwiseArea(
							_opensolid$geometry$OpenSolid_Geometry_Types$Triangle2d(
								{ctor: '_Tuple3', _0: _p4._0, _1: start, _2: end}));
					});
				var segmentAreas = A3(
					_elm_lang$core$List$map2,
					segmentArea,
					{ctor: '::', _0: _p4._1._0, _1: _p5},
					_p5);
				return _elm_lang$core$List$sum(segmentAreas);
			}
		}
	}
};
var _opensolid$geometry$OpenSolid_Polygon2d$area = function (_p6) {
	return _elm_lang$core$Basics$abs(
		_opensolid$geometry$OpenSolid_Polygon2d$counterclockwiseArea(_p6));
};
var _opensolid$geometry$OpenSolid_Polygon2d$clockwiseArea = function (polygon) {
	return 0 - _opensolid$geometry$OpenSolid_Polygon2d$counterclockwiseArea(polygon);
};
var _opensolid$geometry$OpenSolid_Polygon2d$map = function ($function) {
	return function (_p7) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Polygon2d(
			A2(
				_elm_lang$core$List$map,
				$function,
				_opensolid$geometry$OpenSolid_Polygon2d$vertices(_p7)));
	};
};
var _opensolid$geometry$OpenSolid_Polygon2d$scaleAbout = F2(
	function (point, scale) {
		return _opensolid$geometry$OpenSolid_Polygon2d$map(
			A2(_opensolid$geometry$OpenSolid_Point2d$scaleAbout, point, scale));
	});
var _opensolid$geometry$OpenSolid_Polygon2d$rotateAround = F2(
	function (point, angle) {
		return _opensolid$geometry$OpenSolid_Polygon2d$map(
			A2(_opensolid$geometry$OpenSolid_Point2d$rotateAround, point, angle));
	});
var _opensolid$geometry$OpenSolid_Polygon2d$translateBy = function (vector) {
	return _opensolid$geometry$OpenSolid_Polygon2d$map(
		_opensolid$geometry$OpenSolid_Point2d$translateBy(vector));
};
var _opensolid$geometry$OpenSolid_Polygon2d$mirrorAcross = function (axis) {
	return _opensolid$geometry$OpenSolid_Polygon2d$map(
		_opensolid$geometry$OpenSolid_Point2d$mirrorAcross(axis));
};
var _opensolid$geometry$OpenSolid_Polygon2d$relativeTo = function (frame) {
	return _opensolid$geometry$OpenSolid_Polygon2d$map(
		_opensolid$geometry$OpenSolid_Point2d$relativeTo(frame));
};
var _opensolid$geometry$OpenSolid_Polygon2d$placeIn = function (frame) {
	return _opensolid$geometry$OpenSolid_Polygon2d$map(
		_opensolid$geometry$OpenSolid_Point2d$placeIn(frame));
};
var _opensolid$geometry$OpenSolid_Polygon2d$boundingBox = function (polygon) {
	return _opensolid$geometry$OpenSolid_BoundingBox2d$containing(
		_opensolid$geometry$OpenSolid_Polygon2d$vertices(polygon));
};

var _opensolid$geometry$OpenSolid_Bootstrap_Axis3d$direction = function (_p0) {
	var _p1 = _p0;
	return _p1._0.direction;
};
var _opensolid$geometry$OpenSolid_Bootstrap_Axis3d$originPoint = function (_p2) {
	var _p3 = _p2;
	return _p3._0.originPoint;
};

var _opensolid$geometry$OpenSolid_Bootstrap_Plane3d$normalDirection = function (_p0) {
	var _p1 = _p0;
	return _p1._0.normalDirection;
};
var _opensolid$geometry$OpenSolid_Bootstrap_Plane3d$originPoint = function (_p2) {
	var _p3 = _p2;
	return _p3._0.originPoint;
};

var _opensolid$geometry$OpenSolid_Bootstrap_Frame3d$zDirection = function (_p0) {
	var _p1 = _p0;
	return _p1._0.zDirection;
};
var _opensolid$geometry$OpenSolid_Bootstrap_Frame3d$yDirection = function (_p2) {
	var _p3 = _p2;
	return _p3._0.yDirection;
};
var _opensolid$geometry$OpenSolid_Bootstrap_Frame3d$xDirection = function (_p4) {
	var _p5 = _p4;
	return _p5._0.xDirection;
};
var _opensolid$geometry$OpenSolid_Bootstrap_Frame3d$originPoint = function (_p6) {
	var _p7 = _p6;
	return _p7._0.originPoint;
};

var _opensolid$geometry$OpenSolid_Vector3d$mirrorAcross = function (plane) {
	var _p0 = _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components(
		_opensolid$geometry$OpenSolid_Bootstrap_Plane3d$normalDirection(plane));
	var dx = _p0._0;
	var dy = _p0._1;
	var dz = _p0._2;
	var a = 1 - ((2 * dx) * dx);
	var b = 1 - ((2 * dy) * dy);
	var c = 1 - ((2 * dz) * dz);
	var d = (-2 * dy) * dz;
	var e = (-2 * dx) * dz;
	var f = (-2 * dx) * dy;
	return function (_p1) {
		var _p2 = _p1;
		var _p5 = _p2._0._2;
		var _p4 = _p2._0._1;
		var _p3 = _p2._0._0;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
			{ctor: '_Tuple3', _0: ((a * _p3) + (f * _p4)) + (e * _p5), _1: ((f * _p3) + (b * _p4)) + (d * _p5), _2: ((e * _p3) + (d * _p4)) + (c * _p5)});
	};
};
var _opensolid$geometry$OpenSolid_Vector3d$rotateAround = F2(
	function (axis, angle) {
		var halfAngle = 0.5 * angle;
		var sinHalfAngle = _elm_lang$core$Basics$sin(halfAngle);
		var w = _elm_lang$core$Basics$cos(halfAngle);
		var _p6 = _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components(
			_opensolid$geometry$OpenSolid_Bootstrap_Axis3d$direction(axis));
		var dx = _p6._0;
		var dy = _p6._1;
		var dz = _p6._2;
		var x = dx * sinHalfAngle;
		var wx = w * x;
		var xx = x * x;
		var y = dy * sinHalfAngle;
		var wy = w * y;
		var xy = x * y;
		var yy = y * y;
		var a22 = 1 - (2 * (xx + yy));
		var z = dz * sinHalfAngle;
		var wz = w * z;
		var a10 = 2 * (xy + wz);
		var a01 = 2 * (xy - wz);
		var xz = x * z;
		var a20 = 2 * (xz - wy);
		var a02 = 2 * (xz + wy);
		var yz = y * z;
		var a21 = 2 * (yz + wx);
		var a12 = 2 * (yz - wx);
		var zz = z * z;
		var a00 = 1 - (2 * (yy + zz));
		var a11 = 1 - (2 * (xx + zz));
		return function (_p7) {
			var _p8 = _p7;
			var _p11 = _p8._0._2;
			var _p10 = _p8._0._1;
			var _p9 = _p8._0._0;
			return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
				{ctor: '_Tuple3', _0: ((a00 * _p9) + (a01 * _p10)) + (a02 * _p11), _1: ((a10 * _p9) + (a11 * _p10)) + (a12 * _p11), _2: ((a20 * _p9) + (a21 * _p10)) + (a22 * _p11)});
		};
	});
var _opensolid$geometry$OpenSolid_Vector3d$zComponent = function (_p12) {
	var _p13 = _p12;
	return _p13._0._2;
};
var _opensolid$geometry$OpenSolid_Vector3d$yComponent = function (_p14) {
	var _p15 = _p14;
	return _p15._0._1;
};
var _opensolid$geometry$OpenSolid_Vector3d$xComponent = function (_p16) {
	var _p17 = _p16;
	return _p17._0._0;
};
var _opensolid$geometry$OpenSolid_Vector3d$components = function (_p18) {
	var _p19 = _p18;
	return _p19._0;
};
var _opensolid$geometry$OpenSolid_Vector3d$componentIn = F2(
	function (direction, vector) {
		var _p20 = _opensolid$geometry$OpenSolid_Vector3d$components(vector);
		var vx = _p20._0;
		var vy = _p20._1;
		var vz = _p20._2;
		var _p21 = _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components(direction);
		var dx = _p21._0;
		var dy = _p21._1;
		var dz = _p21._2;
		return ((vx * dx) + (vy * dy)) + (vz * dz);
	});
var _opensolid$geometry$OpenSolid_Vector3d$relativeTo = F2(
	function (frame, vector) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
			{
				ctor: '_Tuple3',
				_0: A2(
					_opensolid$geometry$OpenSolid_Vector3d$componentIn,
					_opensolid$geometry$OpenSolid_Bootstrap_Frame3d$xDirection(frame),
					vector),
				_1: A2(
					_opensolid$geometry$OpenSolid_Vector3d$componentIn,
					_opensolid$geometry$OpenSolid_Bootstrap_Frame3d$yDirection(frame),
					vector),
				_2: A2(
					_opensolid$geometry$OpenSolid_Vector3d$componentIn,
					_opensolid$geometry$OpenSolid_Bootstrap_Frame3d$zDirection(frame),
					vector)
			});
	});
var _opensolid$geometry$OpenSolid_Vector3d$projectInto = F2(
	function (sketchPlane, vector) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
			{
				ctor: '_Tuple2',
				_0: A2(
					_opensolid$geometry$OpenSolid_Vector3d$componentIn,
					_opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$xDirection(sketchPlane),
					vector),
				_1: A2(
					_opensolid$geometry$OpenSolid_Vector3d$componentIn,
					_opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$yDirection(sketchPlane),
					vector)
			});
	});
var _opensolid$geometry$OpenSolid_Vector3d$squaredLength = function (vector) {
	var _p22 = _opensolid$geometry$OpenSolid_Vector3d$components(vector);
	var x = _p22._0;
	var y = _p22._1;
	var z = _p22._2;
	return ((x * x) + (y * y)) + (z * z);
};
var _opensolid$geometry$OpenSolid_Vector3d$length = function (vector) {
	return _elm_lang$core$Basics$sqrt(
		_opensolid$geometry$OpenSolid_Vector3d$squaredLength(vector));
};
var _opensolid$geometry$OpenSolid_Vector3d$sum = F2(
	function (firstVector, secondVector) {
		var _p23 = _opensolid$geometry$OpenSolid_Vector3d$components(secondVector);
		var x2 = _p23._0;
		var y2 = _p23._1;
		var z2 = _p23._2;
		var _p24 = _opensolid$geometry$OpenSolid_Vector3d$components(firstVector);
		var x1 = _p24._0;
		var y1 = _p24._1;
		var z1 = _p24._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
			{ctor: '_Tuple3', _0: x1 + x2, _1: y1 + y2, _2: z1 + z2});
	});
var _opensolid$geometry$OpenSolid_Vector3d$difference = F2(
	function (firstVector, secondVector) {
		var _p25 = _opensolid$geometry$OpenSolid_Vector3d$components(secondVector);
		var x2 = _p25._0;
		var y2 = _p25._1;
		var z2 = _p25._2;
		var _p26 = _opensolid$geometry$OpenSolid_Vector3d$components(firstVector);
		var x1 = _p26._0;
		var y1 = _p26._1;
		var z1 = _p26._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
			{ctor: '_Tuple3', _0: x1 - x2, _1: y1 - y2, _2: z1 - z2});
	});
var _opensolid$geometry$OpenSolid_Vector3d$equalWithin = F3(
	function (tolerance, firstVector, secondVector) {
		return _elm_lang$core$Native_Utils.cmp(
			_opensolid$geometry$OpenSolid_Vector3d$squaredLength(
				A2(_opensolid$geometry$OpenSolid_Vector3d$difference, firstVector, secondVector)),
			tolerance * tolerance) < 1;
	});
var _opensolid$geometry$OpenSolid_Vector3d$dotProduct = F2(
	function (firstVector, secondVector) {
		var _p27 = _opensolid$geometry$OpenSolid_Vector3d$components(secondVector);
		var x2 = _p27._0;
		var y2 = _p27._1;
		var z2 = _p27._2;
		var _p28 = _opensolid$geometry$OpenSolid_Vector3d$components(firstVector);
		var x1 = _p28._0;
		var y1 = _p28._1;
		var z1 = _p28._2;
		return ((x1 * x2) + (y1 * y2)) + (z1 * z2);
	});
var _opensolid$geometry$OpenSolid_Vector3d$crossProduct = F2(
	function (firstVector, secondVector) {
		var _p29 = _opensolid$geometry$OpenSolid_Vector3d$components(secondVector);
		var x2 = _p29._0;
		var y2 = _p29._1;
		var z2 = _p29._2;
		var _p30 = _opensolid$geometry$OpenSolid_Vector3d$components(firstVector);
		var x1 = _p30._0;
		var y1 = _p30._1;
		var z1 = _p30._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
			{ctor: '_Tuple3', _0: (y1 * z2) - (z1 * y2), _1: (z1 * x2) - (x1 * z2), _2: (x1 * y2) - (y1 * x2)});
	});
var _opensolid$geometry$OpenSolid_Vector3d$flip = function (vector) {
	var _p31 = _opensolid$geometry$OpenSolid_Vector3d$components(vector);
	var x = _p31._0;
	var y = _p31._1;
	var z = _p31._2;
	return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
		{ctor: '_Tuple3', _0: 0 - x, _1: 0 - y, _2: 0 - z});
};
var _opensolid$geometry$OpenSolid_Vector3d$scaleBy = F2(
	function (scale, vector) {
		var _p32 = _opensolid$geometry$OpenSolid_Vector3d$components(vector);
		var x = _p32._0;
		var y = _p32._1;
		var z = _p32._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
			{ctor: '_Tuple3', _0: x * scale, _1: y * scale, _2: z * scale});
	});
var _opensolid$geometry$OpenSolid_Vector3d$lengthAndDirection = function (vector) {
	var vectorLength = _opensolid$geometry$OpenSolid_Vector3d$length(vector);
	if (_elm_lang$core$Native_Utils.eq(vectorLength, 0.0)) {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		var normalizedVector = A2(_opensolid$geometry$OpenSolid_Vector3d$scaleBy, 1 / vectorLength, vector);
		var vectorDirection = _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
			_opensolid$geometry$OpenSolid_Vector3d$components(normalizedVector));
		return _elm_lang$core$Maybe$Just(
			{ctor: '_Tuple2', _0: vectorLength, _1: vectorDirection});
	}
};
var _opensolid$geometry$OpenSolid_Vector3d$placeIn = F2(
	function (frame, vector) {
		var _p33 = _opensolid$geometry$OpenSolid_Vector3d$components(vector);
		var x = _p33._0;
		var y = _p33._1;
		var z = _p33._2;
		var _p34 = _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components(
			_opensolid$geometry$OpenSolid_Bootstrap_Frame3d$zDirection(frame));
		var x3 = _p34._0;
		var y3 = _p34._1;
		var z3 = _p34._2;
		var _p35 = _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components(
			_opensolid$geometry$OpenSolid_Bootstrap_Frame3d$yDirection(frame));
		var x2 = _p35._0;
		var y2 = _p35._1;
		var z2 = _p35._2;
		var _p36 = _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components(
			_opensolid$geometry$OpenSolid_Bootstrap_Frame3d$xDirection(frame));
		var x1 = _p36._0;
		var y1 = _p36._1;
		var z1 = _p36._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
			{ctor: '_Tuple3', _0: ((x1 * x) + (x2 * y)) + (x3 * z), _1: ((y1 * x) + (y2 * y)) + (y3 * z), _2: ((z1 * x) + (z2 * y)) + (z3 * z)});
	});
var _opensolid$geometry$OpenSolid_Vector3d$interpolateFrom = F3(
	function (v1, v2, t) {
		var _p37 = _opensolid$geometry$OpenSolid_Vector3d$components(v2);
		var x2 = _p37._0;
		var y2 = _p37._1;
		var z2 = _p37._2;
		var _p38 = _opensolid$geometry$OpenSolid_Vector3d$components(v1);
		var x1 = _p38._0;
		var y1 = _p38._1;
		var z1 = _p38._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
			{
				ctor: '_Tuple3',
				_0: A3(_opensolid$geometry$OpenSolid_Scalar$interpolateFrom, x1, x2, t),
				_1: A3(_opensolid$geometry$OpenSolid_Scalar$interpolateFrom, y1, y2, t),
				_2: A3(_opensolid$geometry$OpenSolid_Scalar$interpolateFrom, z1, z2, t)
			});
	});
var _opensolid$geometry$OpenSolid_Vector3d$perpendicularTo = function (vector) {
	var _p39 = _opensolid$geometry$OpenSolid_Vector3d$components(vector);
	var x = _p39._0;
	var y = _p39._1;
	var z = _p39._2;
	var absX = _elm_lang$core$Basics$abs(x);
	var absY = _elm_lang$core$Basics$abs(y);
	var absZ = _elm_lang$core$Basics$abs(z);
	return (_elm_lang$core$Native_Utils.cmp(absX, absY) < 1) ? ((_elm_lang$core$Native_Utils.cmp(absX, absZ) < 1) ? _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
		{ctor: '_Tuple3', _0: 0, _1: 0 - z, _2: y}) : _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
		{ctor: '_Tuple3', _0: 0 - y, _1: x, _2: 0})) : ((_elm_lang$core$Native_Utils.cmp(absY, absZ) < 1) ? _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
		{ctor: '_Tuple3', _0: z, _1: 0, _2: 0 - x}) : _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
		{ctor: '_Tuple3', _0: 0 - y, _1: x, _2: 0}));
};
var _opensolid$geometry$OpenSolid_Vector3d$in_ = F2(
	function (direction, length) {
		var _p40 = _opensolid$geometry$OpenSolid_Bootstrap_Direction3d$components(direction);
		var dx = _p40._0;
		var dy = _p40._1;
		var dz = _p40._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
			{ctor: '_Tuple3', _0: length * dx, _1: length * dy, _2: length * dz});
	});
var _opensolid$geometry$OpenSolid_Vector3d$projectionIn = F2(
	function (direction, vector) {
		return A2(
			_opensolid$geometry$OpenSolid_Vector3d$in_,
			direction,
			A2(_opensolid$geometry$OpenSolid_Vector3d$componentIn, direction, vector));
	});
var _opensolid$geometry$OpenSolid_Vector3d$projectOnto = F2(
	function (plane, vector) {
		return A2(
			_opensolid$geometry$OpenSolid_Vector3d$difference,
			vector,
			A2(
				_opensolid$geometry$OpenSolid_Vector3d$projectionIn,
				_opensolid$geometry$OpenSolid_Bootstrap_Plane3d$normalDirection(plane),
				vector));
	});
var _opensolid$geometry$OpenSolid_Vector3d$zero = _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
	{ctor: '_Tuple3', _0: 0, _1: 0, _2: 0});
var _opensolid$geometry$OpenSolid_Vector3d$direction = function (vector) {
	if (_elm_lang$core$Native_Utils.eq(vector, _opensolid$geometry$OpenSolid_Vector3d$zero)) {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		var normalizedVector = A2(
			_opensolid$geometry$OpenSolid_Vector3d$scaleBy,
			1 / _opensolid$geometry$OpenSolid_Vector3d$length(vector),
			vector);
		return _elm_lang$core$Maybe$Just(
			_opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
				_opensolid$geometry$OpenSolid_Vector3d$components(normalizedVector)));
	}
};
var _opensolid$geometry$OpenSolid_Vector3d$orthonormalize = function (_p41) {
	var _p42 = _p41;
	var _p44 = _p42._2;
	var _p43 = _p42._1;
	return A2(
		_elm_lang$core$Maybe$andThen,
		function (xDirection) {
			var xProjection = A2(_opensolid$geometry$OpenSolid_Vector3d$projectionIn, xDirection, _p43);
			var yVector = A2(_opensolid$geometry$OpenSolid_Vector3d$difference, _p43, xProjection);
			return A2(
				_elm_lang$core$Maybe$andThen,
				function (yDirection) {
					var xProjection = A2(_opensolid$geometry$OpenSolid_Vector3d$projectionIn, xDirection, _p44);
					var yzVector = A2(_opensolid$geometry$OpenSolid_Vector3d$difference, _p44, xProjection);
					var yProjection = A2(_opensolid$geometry$OpenSolid_Vector3d$projectionIn, yDirection, yzVector);
					var zVector = A2(_opensolid$geometry$OpenSolid_Vector3d$difference, yzVector, yProjection);
					return A2(
						_elm_lang$core$Maybe$map,
						function (zDirection) {
							return {ctor: '_Tuple3', _0: xDirection, _1: yDirection, _2: zDirection};
						},
						_opensolid$geometry$OpenSolid_Vector3d$direction(zVector));
				},
				_opensolid$geometry$OpenSolid_Vector3d$direction(yVector));
		},
		_opensolid$geometry$OpenSolid_Vector3d$direction(_p42._0));
};

var _opensolid$geometry$OpenSolid_Direction3d$toVector = function (_p0) {
	var _p1 = _p0;
	return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(_p1._0);
};
var _opensolid$geometry$OpenSolid_Direction3d$scaleBy = F2(
	function (scale, direction) {
		return A2(
			_opensolid$geometry$OpenSolid_Vector3d$scaleBy,
			scale,
			_opensolid$geometry$OpenSolid_Direction3d$toVector(direction));
	});
var _opensolid$geometry$OpenSolid_Direction3d$projectOnto = F2(
	function (plane, direction) {
		return _opensolid$geometry$OpenSolid_Vector3d$direction(
			A2(
				_opensolid$geometry$OpenSolid_Vector3d$projectOnto,
				plane,
				_opensolid$geometry$OpenSolid_Direction3d$toVector(direction)));
	});
var _opensolid$geometry$OpenSolid_Direction3d$projectInto = F2(
	function (sketchPlane, direction) {
		return _opensolid$geometry$OpenSolid_Vector2d$direction(
			A2(
				_opensolid$geometry$OpenSolid_Vector3d$projectInto,
				sketchPlane,
				_opensolid$geometry$OpenSolid_Direction3d$toVector(direction)));
	});
var _opensolid$geometry$OpenSolid_Direction3d$componentIn = F2(
	function (firstDirection, secondDirection) {
		return A2(
			_opensolid$geometry$OpenSolid_Vector3d$componentIn,
			firstDirection,
			_opensolid$geometry$OpenSolid_Direction3d$toVector(secondDirection));
	});
var _opensolid$geometry$OpenSolid_Direction3d$angleFrom = F2(
	function (firstDirection, secondDirection) {
		return _elm_lang$core$Basics$acos(
			A2(_opensolid$geometry$OpenSolid_Direction3d$componentIn, firstDirection, secondDirection));
	});
var _opensolid$geometry$OpenSolid_Direction3d$equalWithin = F3(
	function (angle, firstDirection, secondDirection) {
		return _elm_lang$core$Native_Utils.cmp(
			A2(_opensolid$geometry$OpenSolid_Direction3d$angleFrom, firstDirection, secondDirection),
			angle) < 1;
	});
var _opensolid$geometry$OpenSolid_Direction3d$zComponent = function (_p2) {
	var _p3 = _p2;
	return _p3._0._2;
};
var _opensolid$geometry$OpenSolid_Direction3d$yComponent = function (_p4) {
	var _p5 = _p4;
	return _p5._0._1;
};
var _opensolid$geometry$OpenSolid_Direction3d$xComponent = function (_p6) {
	var _p7 = _p6;
	return _p7._0._0;
};
var _opensolid$geometry$OpenSolid_Direction3d$components = function (_p8) {
	var _p9 = _p8;
	return _p9._0;
};
var _opensolid$geometry$OpenSolid_Direction3d$flip = function (direction) {
	var _p10 = _opensolid$geometry$OpenSolid_Direction3d$components(direction);
	var x = _p10._0;
	var y = _p10._1;
	var z = _p10._2;
	return _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
		{ctor: '_Tuple3', _0: 0 - x, _1: 0 - y, _2: 0 - z});
};
var _opensolid$geometry$OpenSolid_Direction3d$orthogonalize = function (_p11) {
	var _p12 = _p11;
	return _opensolid$geometry$OpenSolid_Vector3d$orthonormalize(
		{
			ctor: '_Tuple3',
			_0: _opensolid$geometry$OpenSolid_Direction3d$toVector(_p12._0),
			_1: _opensolid$geometry$OpenSolid_Direction3d$toVector(_p12._1),
			_2: _opensolid$geometry$OpenSolid_Direction3d$toVector(_p12._2)
		});
};
var _opensolid$geometry$OpenSolid_Direction3d$negativeZ = _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
	{ctor: '_Tuple3', _0: 0, _1: 0, _2: -1});
var _opensolid$geometry$OpenSolid_Direction3d$positiveZ = _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
	{ctor: '_Tuple3', _0: 0, _1: 0, _2: 1});
var _opensolid$geometry$OpenSolid_Direction3d$negativeY = _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
	{ctor: '_Tuple3', _0: 0, _1: -1, _2: 0});
var _opensolid$geometry$OpenSolid_Direction3d$positiveY = _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
	{ctor: '_Tuple3', _0: 0, _1: 1, _2: 0});
var _opensolid$geometry$OpenSolid_Direction3d$negativeX = _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
	{ctor: '_Tuple3', _0: -1, _1: 0, _2: 0});
var _opensolid$geometry$OpenSolid_Direction3d$positiveX = _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
	{ctor: '_Tuple3', _0: 1, _1: 0, _2: 0});
var _opensolid$geometry$OpenSolid_Direction3d$z = _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
	{ctor: '_Tuple3', _0: 0, _1: 0, _2: 1});
var _opensolid$geometry$OpenSolid_Direction3d$y = _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
	{ctor: '_Tuple3', _0: 0, _1: 1, _2: 0});
var _opensolid$geometry$OpenSolid_Direction3d$x = _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
	{ctor: '_Tuple3', _0: 1, _1: 0, _2: 0});
var _opensolid$geometry$OpenSolid_Direction3d$toDirection = function (_p13) {
	var _p14 = _p13;
	return _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(_p14._0);
};
var _opensolid$geometry$OpenSolid_Direction3d$perpendicularTo = function (direction) {
	var perpendicularVector = _opensolid$geometry$OpenSolid_Vector3d$perpendicularTo(
		_opensolid$geometry$OpenSolid_Direction3d$toVector(direction));
	var length = _opensolid$geometry$OpenSolid_Vector3d$length(perpendicularVector);
	var normalizedVector = A2(_opensolid$geometry$OpenSolid_Vector3d$scaleBy, 1 / length, perpendicularVector);
	return _opensolid$geometry$OpenSolid_Direction3d$toDirection(normalizedVector);
};
var _opensolid$geometry$OpenSolid_Direction3d$perpendicularBasis = function (direction) {
	var xDirection = _opensolid$geometry$OpenSolid_Direction3d$perpendicularTo(direction);
	var yDirection = _opensolid$geometry$OpenSolid_Direction3d$toDirection(
		A2(
			_opensolid$geometry$OpenSolid_Vector3d$crossProduct,
			_opensolid$geometry$OpenSolid_Direction3d$toVector(direction),
			_opensolid$geometry$OpenSolid_Direction3d$toVector(xDirection)));
	return {ctor: '_Tuple2', _0: xDirection, _1: yDirection};
};
var _opensolid$geometry$OpenSolid_Direction3d$rotateAround = F3(
	function (axis, angle, direction) {
		return _opensolid$geometry$OpenSolid_Direction3d$toDirection(
			A3(
				_opensolid$geometry$OpenSolid_Vector3d$rotateAround,
				axis,
				angle,
				_opensolid$geometry$OpenSolid_Direction3d$toVector(direction)));
	});
var _opensolid$geometry$OpenSolid_Direction3d$mirrorAcross = F2(
	function (plane, direction) {
		return _opensolid$geometry$OpenSolid_Direction3d$toDirection(
			A2(
				_opensolid$geometry$OpenSolid_Vector3d$mirrorAcross,
				plane,
				_opensolid$geometry$OpenSolid_Direction3d$toVector(direction)));
	});
var _opensolid$geometry$OpenSolid_Direction3d$relativeTo = F2(
	function (frame, direction) {
		return _opensolid$geometry$OpenSolid_Direction3d$toDirection(
			A2(
				_opensolid$geometry$OpenSolid_Vector3d$relativeTo,
				frame,
				_opensolid$geometry$OpenSolid_Direction3d$toVector(direction)));
	});
var _opensolid$geometry$OpenSolid_Direction3d$placeIn = F2(
	function (frame, direction) {
		return _opensolid$geometry$OpenSolid_Direction3d$toDirection(
			A2(
				_opensolid$geometry$OpenSolid_Vector3d$placeIn,
				frame,
				_opensolid$geometry$OpenSolid_Direction3d$toVector(direction)));
	});

var _opensolid$geometry$OpenSolid_Point3d$zCoordinate = function (_p0) {
	var _p1 = _p0;
	return _p1._0._2;
};
var _opensolid$geometry$OpenSolid_Point3d$yCoordinate = function (_p2) {
	var _p3 = _p2;
	return _p3._0._1;
};
var _opensolid$geometry$OpenSolid_Point3d$xCoordinate = function (_p4) {
	var _p5 = _p4;
	return _p5._0._0;
};
var _opensolid$geometry$OpenSolid_Point3d$coordinates = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};
var _opensolid$geometry$OpenSolid_Point3d$vectorFrom = F2(
	function (firstPoint, secondPoint) {
		var _p8 = _opensolid$geometry$OpenSolid_Point3d$coordinates(secondPoint);
		var x2 = _p8._0;
		var y2 = _p8._1;
		var z2 = _p8._2;
		var _p9 = _opensolid$geometry$OpenSolid_Point3d$coordinates(firstPoint);
		var x1 = _p9._0;
		var y1 = _p9._1;
		var z1 = _p9._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
			{ctor: '_Tuple3', _0: x2 - x1, _1: y2 - y1, _2: z2 - z1});
	});
var _opensolid$geometry$OpenSolid_Point3d$directionFrom = F2(
	function (firstPoint, secondPoint) {
		return _opensolid$geometry$OpenSolid_Vector3d$direction(
			A2(_opensolid$geometry$OpenSolid_Point3d$vectorFrom, firstPoint, secondPoint));
	});
var _opensolid$geometry$OpenSolid_Point3d$squaredDistanceFrom = F2(
	function (firstPoint, secondPoint) {
		return _opensolid$geometry$OpenSolid_Vector3d$squaredLength(
			A2(_opensolid$geometry$OpenSolid_Point3d$vectorFrom, firstPoint, secondPoint));
	});
var _opensolid$geometry$OpenSolid_Point3d$equalWithin = F3(
	function (tolerance, firstPoint, secondPoint) {
		return _elm_lang$core$Native_Utils.cmp(
			A2(_opensolid$geometry$OpenSolid_Point3d$squaredDistanceFrom, firstPoint, secondPoint),
			tolerance * tolerance) < 1;
	});
var _opensolid$geometry$OpenSolid_Point3d$distanceFrom = F2(
	function (firstPoint, secondPoint) {
		return _elm_lang$core$Basics$sqrt(
			A2(_opensolid$geometry$OpenSolid_Point3d$squaredDistanceFrom, firstPoint, secondPoint));
	});
var _opensolid$geometry$OpenSolid_Point3d$distanceAlong = F2(
	function (axis, point) {
		return A2(
			_opensolid$geometry$OpenSolid_Vector3d$componentIn,
			_opensolid$geometry$OpenSolid_Bootstrap_Axis3d$direction(axis),
			A2(
				_opensolid$geometry$OpenSolid_Point3d$vectorFrom,
				_opensolid$geometry$OpenSolid_Bootstrap_Axis3d$originPoint(axis),
				point));
	});
var _opensolid$geometry$OpenSolid_Point3d$squaredRadialDistanceFrom = F2(
	function (axis, point) {
		return _opensolid$geometry$OpenSolid_Vector3d$squaredLength(
			A2(
				_opensolid$geometry$OpenSolid_Vector3d$crossProduct,
				_opensolid$geometry$OpenSolid_Direction3d$toVector(
					_opensolid$geometry$OpenSolid_Bootstrap_Axis3d$direction(axis)),
				A2(
					_opensolid$geometry$OpenSolid_Point3d$vectorFrom,
					_opensolid$geometry$OpenSolid_Bootstrap_Axis3d$originPoint(axis),
					point)));
	});
var _opensolid$geometry$OpenSolid_Point3d$radialDistanceFrom = F2(
	function (axis, point) {
		return _elm_lang$core$Basics$sqrt(
			A2(_opensolid$geometry$OpenSolid_Point3d$squaredRadialDistanceFrom, axis, point));
	});
var _opensolid$geometry$OpenSolid_Point3d$relativeTo = F2(
	function (frame, point) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Point3d(
			_opensolid$geometry$OpenSolid_Vector3d$components(
				A2(
					_opensolid$geometry$OpenSolid_Vector3d$relativeTo,
					frame,
					A2(
						_opensolid$geometry$OpenSolid_Point3d$vectorFrom,
						_opensolid$geometry$OpenSolid_Bootstrap_Frame3d$originPoint(frame),
						point))));
	});
var _opensolid$geometry$OpenSolid_Point3d$signedDistanceFrom = F2(
	function (plane, point) {
		var _p10 = _opensolid$geometry$OpenSolid_Direction3d$components(
			_opensolid$geometry$OpenSolid_Bootstrap_Plane3d$normalDirection(plane));
		var nx = _p10._0;
		var ny = _p10._1;
		var nz = _p10._2;
		var _p11 = _opensolid$geometry$OpenSolid_Point3d$coordinates(
			_opensolid$geometry$OpenSolid_Bootstrap_Plane3d$originPoint(plane));
		var x0 = _p11._0;
		var y0 = _p11._1;
		var z0 = _p11._2;
		var _p12 = _opensolid$geometry$OpenSolid_Point3d$coordinates(point);
		var x = _p12._0;
		var y = _p12._1;
		var z = _p12._2;
		return (((x - x0) * nx) + ((y - y0) * ny)) + ((z - z0) * nz);
	});
var _opensolid$geometry$OpenSolid_Point3d$translateBy = F2(
	function (vector, point) {
		var _p13 = _opensolid$geometry$OpenSolid_Point3d$coordinates(point);
		var px = _p13._0;
		var py = _p13._1;
		var pz = _p13._2;
		var _p14 = _opensolid$geometry$OpenSolid_Vector3d$components(vector);
		var vx = _p14._0;
		var vy = _p14._1;
		var vz = _p14._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Point3d(
			{ctor: '_Tuple3', _0: px + vx, _1: py + vy, _2: pz + vz});
	});
var _opensolid$geometry$OpenSolid_Point3d$projectOnto = F2(
	function (plane, point) {
		var signedDistance = A2(_opensolid$geometry$OpenSolid_Point3d$signedDistanceFrom, plane, point);
		var displacement = A2(
			_opensolid$geometry$OpenSolid_Vector3d$in_,
			_opensolid$geometry$OpenSolid_Bootstrap_Plane3d$normalDirection(plane),
			0 - signedDistance);
		return A2(_opensolid$geometry$OpenSolid_Point3d$translateBy, displacement, point);
	});
var _opensolid$geometry$OpenSolid_Point3d$projectInto = F2(
	function (sketchPlane, point) {
		var _p15 = _opensolid$geometry$OpenSolid_Direction3d$components(
			_opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$yDirection(sketchPlane));
		var vx = _p15._0;
		var vy = _p15._1;
		var vz = _p15._2;
		var _p16 = _opensolid$geometry$OpenSolid_Direction3d$components(
			_opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$xDirection(sketchPlane));
		var ux = _p16._0;
		var uy = _p16._1;
		var uz = _p16._2;
		var _p17 = _opensolid$geometry$OpenSolid_Point3d$coordinates(
			_opensolid$geometry$OpenSolid_Bootstrap_SketchPlane3d$originPoint(sketchPlane));
		var x0 = _p17._0;
		var y0 = _p17._1;
		var z0 = _p17._2;
		var _p18 = _opensolid$geometry$OpenSolid_Point3d$coordinates(point);
		var x = _p18._0;
		var y = _p18._1;
		var z = _p18._2;
		var dx = x - x0;
		var dy = y - y0;
		var dz = z - z0;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
			{ctor: '_Tuple2', _0: ((dx * ux) + (dy * uy)) + (dz * uz), _1: ((dx * vx) + (dy * vy)) + (dz * vz)});
	});
var _opensolid$geometry$OpenSolid_Point3d$hull = F2(
	function (firstPoint, secondPoint) {
		var _p19 = _opensolid$geometry$OpenSolid_Point3d$coordinates(secondPoint);
		var x2 = _p19._0;
		var y2 = _p19._1;
		var z2 = _p19._2;
		var _p20 = _opensolid$geometry$OpenSolid_Point3d$coordinates(firstPoint);
		var x1 = _p20._0;
		var y1 = _p20._1;
		var z1 = _p20._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$BoundingBox3d(
			{
				minX: A2(_elm_lang$core$Basics$min, x1, x2),
				maxX: A2(_elm_lang$core$Basics$max, x1, x2),
				minY: A2(_elm_lang$core$Basics$min, y1, y2),
				maxY: A2(_elm_lang$core$Basics$max, y1, y2),
				minZ: A2(_elm_lang$core$Basics$min, z1, z2),
				maxZ: A2(_elm_lang$core$Basics$max, z1, z2)
			});
	});
var _opensolid$geometry$OpenSolid_Point3d$on = F2(
	function (sketchPlane, coordinates) {
		return A2(
			_opensolid$geometry$OpenSolid_Point2d$placeOnto,
			sketchPlane,
			_opensolid$geometry$OpenSolid_Geometry_Types$Point2d(coordinates));
	});
var _opensolid$geometry$OpenSolid_Point3d$along = F2(
	function (axis, distance) {
		return A2(
			_opensolid$geometry$OpenSolid_Point3d$translateBy,
			A2(
				_opensolid$geometry$OpenSolid_Vector3d$in_,
				_opensolid$geometry$OpenSolid_Bootstrap_Axis3d$direction(axis),
				distance),
			_opensolid$geometry$OpenSolid_Bootstrap_Axis3d$originPoint(axis));
	});
var _opensolid$geometry$OpenSolid_Point3d$projectRadiallyOnto = F2(
	function (axis, point) {
		return A2(
			_opensolid$geometry$OpenSolid_Point3d$along,
			axis,
			A2(_opensolid$geometry$OpenSolid_Point3d$distanceAlong, axis, point));
	});
var _opensolid$geometry$OpenSolid_Point3d$interpolateFrom = F3(
	function (p1, p2, t) {
		var _p21 = _opensolid$geometry$OpenSolid_Point3d$coordinates(p2);
		var x2 = _p21._0;
		var y2 = _p21._1;
		var z2 = _p21._2;
		var _p22 = _opensolid$geometry$OpenSolid_Point3d$coordinates(p1);
		var x1 = _p22._0;
		var y1 = _p22._1;
		var z1 = _p22._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$Point3d(
			{
				ctor: '_Tuple3',
				_0: A3(_opensolid$geometry$OpenSolid_Scalar$interpolateFrom, x1, x2, t),
				_1: A3(_opensolid$geometry$OpenSolid_Scalar$interpolateFrom, y1, y2, t),
				_2: A3(_opensolid$geometry$OpenSolid_Scalar$interpolateFrom, z1, z2, t)
			});
	});
var _opensolid$geometry$OpenSolid_Point3d$interpolate = _opensolid$geometry$OpenSolid_Point3d$interpolateFrom;
var _opensolid$geometry$OpenSolid_Point3d$midpoint = F2(
	function (firstPoint, secondPoint) {
		return A3(_opensolid$geometry$OpenSolid_Point3d$interpolateFrom, firstPoint, secondPoint, 0.5);
	});
var _opensolid$geometry$OpenSolid_Point3d$origin = _opensolid$geometry$OpenSolid_Geometry_Types$Point3d(
	{ctor: '_Tuple3', _0: 0, _1: 0, _2: 0});
var _opensolid$geometry$OpenSolid_Point3d$addTo = _elm_lang$core$Basics$flip(_opensolid$geometry$OpenSolid_Point3d$translateBy);
var _opensolid$geometry$OpenSolid_Point3d$scaleAbout = F3(
	function (centerPoint, scale, point) {
		return A2(
			_opensolid$geometry$OpenSolid_Point3d$addTo,
			centerPoint,
			A2(
				_opensolid$geometry$OpenSolid_Vector3d$scaleBy,
				scale,
				A2(_opensolid$geometry$OpenSolid_Point3d$vectorFrom, centerPoint, point)));
	});
var _opensolid$geometry$OpenSolid_Point3d$rotateAround = F3(
	function (axis, angle, point) {
		var originPoint = _opensolid$geometry$OpenSolid_Bootstrap_Axis3d$originPoint(axis);
		return A2(
			_opensolid$geometry$OpenSolid_Point3d$addTo,
			originPoint,
			A3(
				_opensolid$geometry$OpenSolid_Vector3d$rotateAround,
				axis,
				angle,
				A2(_opensolid$geometry$OpenSolid_Point3d$vectorFrom, originPoint, point)));
	});
var _opensolid$geometry$OpenSolid_Point3d$mirrorAcross = F2(
	function (plane, point) {
		var originPoint = _opensolid$geometry$OpenSolid_Bootstrap_Plane3d$originPoint(plane);
		return A2(
			_opensolid$geometry$OpenSolid_Point3d$addTo,
			originPoint,
			A2(
				_opensolid$geometry$OpenSolid_Vector3d$mirrorAcross,
				plane,
				A2(_opensolid$geometry$OpenSolid_Point3d$vectorFrom, originPoint, point)));
	});
var _opensolid$geometry$OpenSolid_Point3d$placeIn = F2(
	function (frame, point) {
		return A2(
			_opensolid$geometry$OpenSolid_Point3d$addTo,
			_opensolid$geometry$OpenSolid_Bootstrap_Frame3d$originPoint(frame),
			A2(
				_opensolid$geometry$OpenSolid_Vector3d$placeIn,
				frame,
				_opensolid$geometry$OpenSolid_Geometry_Types$Vector3d(
					_opensolid$geometry$OpenSolid_Point3d$coordinates(point))));
	});
var _opensolid$geometry$OpenSolid_Point3d$in_ = F2(
	function (frame, coordinates) {
		return A2(
			_opensolid$geometry$OpenSolid_Point3d$placeIn,
			frame,
			_opensolid$geometry$OpenSolid_Geometry_Types$Point3d(coordinates));
	});

var _opensolid$geometry$OpenSolid_Axis3d$direction = function (_p0) {
	var _p1 = _p0;
	return _p1._0.direction;
};
var _opensolid$geometry$OpenSolid_Axis3d$moveTo = F2(
	function (newOrigin, axis) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
			{
				originPoint: newOrigin,
				direction: _opensolid$geometry$OpenSolid_Axis3d$direction(axis)
			});
	});
var _opensolid$geometry$OpenSolid_Axis3d$originPoint = function (_p2) {
	var _p3 = _p2;
	return _p3._0.originPoint;
};
var _opensolid$geometry$OpenSolid_Axis3d$flip = function (axis) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
		{
			originPoint: _opensolid$geometry$OpenSolid_Axis3d$originPoint(axis),
			direction: _opensolid$geometry$OpenSolid_Direction3d$flip(
				_opensolid$geometry$OpenSolid_Axis3d$direction(axis))
		});
};
var _opensolid$geometry$OpenSolid_Axis3d$rotateAround = F2(
	function (otherAxis, angle) {
		var rotateDirection = A2(_opensolid$geometry$OpenSolid_Direction3d$rotateAround, otherAxis, angle);
		var rotatePoint = A2(_opensolid$geometry$OpenSolid_Point3d$rotateAround, otherAxis, angle);
		return function (axis) {
			return _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
				{
					originPoint: rotatePoint(
						_opensolid$geometry$OpenSolid_Axis3d$originPoint(axis)),
					direction: rotateDirection(
						_opensolid$geometry$OpenSolid_Axis3d$direction(axis))
				});
		};
	});
var _opensolid$geometry$OpenSolid_Axis3d$translateBy = F2(
	function (vector, axis) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
			{
				originPoint: A2(
					_opensolid$geometry$OpenSolid_Point3d$translateBy,
					vector,
					_opensolid$geometry$OpenSolid_Axis3d$originPoint(axis)),
				direction: _opensolid$geometry$OpenSolid_Axis3d$direction(axis)
			});
	});
var _opensolid$geometry$OpenSolid_Axis3d$mirrorAcross = function (plane) {
	var mirrorDirection = _opensolid$geometry$OpenSolid_Direction3d$mirrorAcross(plane);
	var mirrorPoint = _opensolid$geometry$OpenSolid_Point3d$mirrorAcross(plane);
	return function (axis) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
			{
				originPoint: mirrorPoint(
					_opensolid$geometry$OpenSolid_Axis3d$originPoint(axis)),
				direction: mirrorDirection(
					_opensolid$geometry$OpenSolid_Axis3d$direction(axis))
			});
	};
};
var _opensolid$geometry$OpenSolid_Axis3d$projectOnto = F2(
	function (plane, axis) {
		var projectedOrigin = A2(
			_opensolid$geometry$OpenSolid_Point3d$projectOnto,
			plane,
			_opensolid$geometry$OpenSolid_Axis3d$originPoint(axis));
		var toAxis = function (direction) {
			return _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
				{originPoint: projectedOrigin, direction: direction});
		};
		return A2(
			_elm_lang$core$Maybe$map,
			toAxis,
			A2(
				_opensolid$geometry$OpenSolid_Direction3d$projectOnto,
				plane,
				_opensolid$geometry$OpenSolid_Axis3d$direction(axis)));
	});
var _opensolid$geometry$OpenSolid_Axis3d$relativeTo = function (frame) {
	var relativeDirection = _opensolid$geometry$OpenSolid_Direction3d$relativeTo(frame);
	var relativePoint = _opensolid$geometry$OpenSolid_Point3d$relativeTo(frame);
	return function (axis) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
			{
				originPoint: relativePoint(
					_opensolid$geometry$OpenSolid_Axis3d$originPoint(axis)),
				direction: relativeDirection(
					_opensolid$geometry$OpenSolid_Axis3d$direction(axis))
			});
	};
};
var _opensolid$geometry$OpenSolid_Axis3d$placeIn = function (frame) {
	var placeDirection = _opensolid$geometry$OpenSolid_Direction3d$placeIn(frame);
	var placePoint = _opensolid$geometry$OpenSolid_Point3d$placeIn(frame);
	return function (axis) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
			{
				originPoint: placePoint(
					_opensolid$geometry$OpenSolid_Axis3d$originPoint(axis)),
				direction: placeDirection(
					_opensolid$geometry$OpenSolid_Axis3d$direction(axis))
			});
	};
};
var _opensolid$geometry$OpenSolid_Axis3d$projectInto = F2(
	function (sketchPlane, axis) {
		var projectedOrigin = A2(
			_opensolid$geometry$OpenSolid_Point3d$projectInto,
			sketchPlane,
			_opensolid$geometry$OpenSolid_Axis3d$originPoint(axis));
		var toAxis = function (direction) {
			return _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d(
				{originPoint: projectedOrigin, direction: direction});
		};
		return A2(
			_elm_lang$core$Maybe$map,
			toAxis,
			A2(
				_opensolid$geometry$OpenSolid_Direction3d$projectInto,
				sketchPlane,
				_opensolid$geometry$OpenSolid_Axis3d$direction(axis)));
	});
var _opensolid$geometry$OpenSolid_Axis3d$z = _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
	{originPoint: _opensolid$geometry$OpenSolid_Point3d$origin, direction: _opensolid$geometry$OpenSolid_Direction3d$z});
var _opensolid$geometry$OpenSolid_Axis3d$y = _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
	{originPoint: _opensolid$geometry$OpenSolid_Point3d$origin, direction: _opensolid$geometry$OpenSolid_Direction3d$y});
var _opensolid$geometry$OpenSolid_Axis3d$x = _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
	{originPoint: _opensolid$geometry$OpenSolid_Point3d$origin, direction: _opensolid$geometry$OpenSolid_Direction3d$x});

var _opensolid$geometry$OpenSolid_SketchPlane3d$yDirection = function (_p0) {
	var _p1 = _p0;
	return _p1._0.yDirection;
};
var _opensolid$geometry$OpenSolid_SketchPlane3d$xDirection = function (_p2) {
	var _p3 = _p2;
	return _p3._0.xDirection;
};
var _opensolid$geometry$OpenSolid_SketchPlane3d$normalDirection = function (sketchPlane) {
	var normalVector = A2(
		_opensolid$geometry$OpenSolid_Vector3d$crossProduct,
		_opensolid$geometry$OpenSolid_Direction3d$toVector(
			_opensolid$geometry$OpenSolid_SketchPlane3d$xDirection(sketchPlane)),
		_opensolid$geometry$OpenSolid_Direction3d$toVector(
			_opensolid$geometry$OpenSolid_SketchPlane3d$yDirection(sketchPlane)));
	return _opensolid$geometry$OpenSolid_Geometry_Types$Direction3d(
		_opensolid$geometry$OpenSolid_Vector3d$components(normalVector));
};
var _opensolid$geometry$OpenSolid_SketchPlane3d$moveTo = F2(
	function (newOrigin, sketchPlane) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
			{
				originPoint: newOrigin,
				xDirection: _opensolid$geometry$OpenSolid_SketchPlane3d$xDirection(sketchPlane),
				yDirection: _opensolid$geometry$OpenSolid_SketchPlane3d$yDirection(sketchPlane)
			});
	});
var _opensolid$geometry$OpenSolid_SketchPlane3d$originPoint = function (_p4) {
	var _p5 = _p4;
	return _p5._0.originPoint;
};
var _opensolid$geometry$OpenSolid_SketchPlane3d$xAxis = function (sketchPlane) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
		{
			originPoint: _opensolid$geometry$OpenSolid_SketchPlane3d$originPoint(sketchPlane),
			direction: _opensolid$geometry$OpenSolid_SketchPlane3d$xDirection(sketchPlane)
		});
};
var _opensolid$geometry$OpenSolid_SketchPlane3d$yAxis = function (sketchPlane) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
		{
			originPoint: _opensolid$geometry$OpenSolid_SketchPlane3d$originPoint(sketchPlane),
			direction: _opensolid$geometry$OpenSolid_SketchPlane3d$yDirection(sketchPlane)
		});
};
var _opensolid$geometry$OpenSolid_SketchPlane3d$normalAxis = function (sketchPlane) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
		{
			originPoint: _opensolid$geometry$OpenSolid_SketchPlane3d$originPoint(sketchPlane),
			direction: _opensolid$geometry$OpenSolid_SketchPlane3d$normalDirection(sketchPlane)
		});
};
var _opensolid$geometry$OpenSolid_SketchPlane3d$plane = function (sketchPlane) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$Plane3d(
		{
			originPoint: _opensolid$geometry$OpenSolid_SketchPlane3d$originPoint(sketchPlane),
			normalDirection: _opensolid$geometry$OpenSolid_SketchPlane3d$normalDirection(sketchPlane)
		});
};
var _opensolid$geometry$OpenSolid_SketchPlane3d$flipX = function (sketchPlane) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
		{
			originPoint: _opensolid$geometry$OpenSolid_SketchPlane3d$originPoint(sketchPlane),
			xDirection: _opensolid$geometry$OpenSolid_Direction3d$flip(
				_opensolid$geometry$OpenSolid_SketchPlane3d$xDirection(sketchPlane)),
			yDirection: _opensolid$geometry$OpenSolid_SketchPlane3d$yDirection(sketchPlane)
		});
};
var _opensolid$geometry$OpenSolid_SketchPlane3d$flipY = function (sketchPlane) {
	return _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
		{
			originPoint: _opensolid$geometry$OpenSolid_SketchPlane3d$originPoint(sketchPlane),
			xDirection: _opensolid$geometry$OpenSolid_SketchPlane3d$xDirection(sketchPlane),
			yDirection: _opensolid$geometry$OpenSolid_Direction3d$flip(
				_opensolid$geometry$OpenSolid_SketchPlane3d$yDirection(sketchPlane))
		});
};
var _opensolid$geometry$OpenSolid_SketchPlane3d$rotateAround = F2(
	function (axis, angle) {
		var rotateDirection = A2(_opensolid$geometry$OpenSolid_Direction3d$rotateAround, axis, angle);
		var rotatePoint = A2(_opensolid$geometry$OpenSolid_Point3d$rotateAround, axis, angle);
		return function (sketchPlane) {
			return _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
				{
					originPoint: rotatePoint(
						_opensolid$geometry$OpenSolid_SketchPlane3d$originPoint(sketchPlane)),
					xDirection: rotateDirection(
						_opensolid$geometry$OpenSolid_SketchPlane3d$xDirection(sketchPlane)),
					yDirection: rotateDirection(
						_opensolid$geometry$OpenSolid_SketchPlane3d$yDirection(sketchPlane))
				});
		};
	});
var _opensolid$geometry$OpenSolid_SketchPlane3d$rotateAroundOwn = F3(
	function (axis, angle, sketchPlane) {
		return A3(
			_opensolid$geometry$OpenSolid_SketchPlane3d$rotateAround,
			axis(sketchPlane),
			angle,
			sketchPlane);
	});
var _opensolid$geometry$OpenSolid_SketchPlane3d$translateBy = F2(
	function (vector, sketchPlane) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
			{
				originPoint: A2(
					_opensolid$geometry$OpenSolid_Point3d$translateBy,
					vector,
					_opensolid$geometry$OpenSolid_SketchPlane3d$originPoint(sketchPlane)),
				xDirection: _opensolid$geometry$OpenSolid_SketchPlane3d$xDirection(sketchPlane),
				yDirection: _opensolid$geometry$OpenSolid_SketchPlane3d$yDirection(sketchPlane)
			});
	});
var _opensolid$geometry$OpenSolid_SketchPlane3d$translateAlongOwn = F3(
	function (axis, distance, frame) {
		var direction = _opensolid$geometry$OpenSolid_Axis3d$direction(
			axis(frame));
		return A2(
			_opensolid$geometry$OpenSolid_SketchPlane3d$translateBy,
			A2(_opensolid$geometry$OpenSolid_Vector3d$in_, direction, distance),
			frame);
	});
var _opensolid$geometry$OpenSolid_SketchPlane3d$mirrorAcross = function (plane) {
	var mirrorDirection = _opensolid$geometry$OpenSolid_Direction3d$mirrorAcross(plane);
	var mirrorPoint = _opensolid$geometry$OpenSolid_Point3d$mirrorAcross(plane);
	return function (sketchPlane) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
			{
				originPoint: mirrorPoint(
					_opensolid$geometry$OpenSolid_SketchPlane3d$originPoint(sketchPlane)),
				xDirection: mirrorDirection(
					_opensolid$geometry$OpenSolid_SketchPlane3d$xDirection(sketchPlane)),
				yDirection: mirrorDirection(
					_opensolid$geometry$OpenSolid_SketchPlane3d$yDirection(sketchPlane))
			});
	};
};
var _opensolid$geometry$OpenSolid_SketchPlane3d$relativeTo = function (frame) {
	var relativeDirection = _opensolid$geometry$OpenSolid_Direction3d$relativeTo(frame);
	var relativePoint = _opensolid$geometry$OpenSolid_Point3d$relativeTo(frame);
	return function (sketchPlane) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
			{
				originPoint: relativePoint(
					_opensolid$geometry$OpenSolid_SketchPlane3d$originPoint(sketchPlane)),
				xDirection: relativeDirection(
					_opensolid$geometry$OpenSolid_SketchPlane3d$xDirection(sketchPlane)),
				yDirection: relativeDirection(
					_opensolid$geometry$OpenSolid_SketchPlane3d$yDirection(sketchPlane))
			});
	};
};
var _opensolid$geometry$OpenSolid_SketchPlane3d$placeIn = function (frame) {
	var placeDirection = _opensolid$geometry$OpenSolid_Direction3d$placeIn(frame);
	var placePoint = _opensolid$geometry$OpenSolid_Point3d$placeIn(frame);
	return function (sketchPlane) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
			{
				originPoint: placePoint(
					_opensolid$geometry$OpenSolid_SketchPlane3d$originPoint(sketchPlane)),
				xDirection: placeDirection(
					_opensolid$geometry$OpenSolid_SketchPlane3d$xDirection(sketchPlane)),
				yDirection: placeDirection(
					_opensolid$geometry$OpenSolid_SketchPlane3d$yDirection(sketchPlane))
			});
	};
};
var _opensolid$geometry$OpenSolid_SketchPlane3d$throughPoints = F3(
	function (firstPoint, secondPoint, thirdPoint) {
		return A2(
			_elm_lang$core$Maybe$andThen,
			function (xDirection) {
				var xDirectionVector = _opensolid$geometry$OpenSolid_Direction3d$toVector(xDirection);
				var secondCandidate = A2(_opensolid$geometry$OpenSolid_Point3d$vectorFrom, secondPoint, thirdPoint);
				var secondSquaredLength = _opensolid$geometry$OpenSolid_Vector3d$squaredLength(secondCandidate);
				var firstCandidate = A2(_opensolid$geometry$OpenSolid_Point3d$vectorFrom, firstPoint, thirdPoint);
				var firstSquaredLength = _opensolid$geometry$OpenSolid_Vector3d$squaredLength(firstCandidate);
				var chosenVector = (_elm_lang$core$Native_Utils.cmp(firstSquaredLength, secondSquaredLength) < 1) ? firstCandidate : secondCandidate;
				var normalVector = A2(_opensolid$geometry$OpenSolid_Vector3d$crossProduct, xDirectionVector, chosenVector);
				var yVector = A2(_opensolid$geometry$OpenSolid_Vector3d$crossProduct, normalVector, xDirectionVector);
				return A2(
					_elm_lang$core$Maybe$map,
					function (yDirection) {
						return _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
							{originPoint: firstPoint, xDirection: xDirection, yDirection: yDirection});
					},
					_opensolid$geometry$OpenSolid_Vector3d$direction(yVector));
			},
			_opensolid$geometry$OpenSolid_Vector3d$direction(
				A2(_opensolid$geometry$OpenSolid_Point3d$vectorFrom, firstPoint, secondPoint)));
	});
var _opensolid$geometry$OpenSolid_SketchPlane3d$xz = _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
	{originPoint: _opensolid$geometry$OpenSolid_Point3d$origin, xDirection: _opensolid$geometry$OpenSolid_Direction3d$x, yDirection: _opensolid$geometry$OpenSolid_Direction3d$z});
var _opensolid$geometry$OpenSolid_SketchPlane3d$zx = _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
	{originPoint: _opensolid$geometry$OpenSolid_Point3d$origin, xDirection: _opensolid$geometry$OpenSolid_Direction3d$z, yDirection: _opensolid$geometry$OpenSolid_Direction3d$x});
var _opensolid$geometry$OpenSolid_SketchPlane3d$zy = _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
	{originPoint: _opensolid$geometry$OpenSolid_Point3d$origin, xDirection: _opensolid$geometry$OpenSolid_Direction3d$z, yDirection: _opensolid$geometry$OpenSolid_Direction3d$y});
var _opensolid$geometry$OpenSolid_SketchPlane3d$yz = _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
	{originPoint: _opensolid$geometry$OpenSolid_Point3d$origin, xDirection: _opensolid$geometry$OpenSolid_Direction3d$y, yDirection: _opensolid$geometry$OpenSolid_Direction3d$z});
var _opensolid$geometry$OpenSolid_SketchPlane3d$yx = _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
	{originPoint: _opensolid$geometry$OpenSolid_Point3d$origin, xDirection: _opensolid$geometry$OpenSolid_Direction3d$y, yDirection: _opensolid$geometry$OpenSolid_Direction3d$x});
var _opensolid$geometry$OpenSolid_SketchPlane3d$xy = _opensolid$geometry$OpenSolid_Geometry_Types$SketchPlane3d(
	{originPoint: _opensolid$geometry$OpenSolid_Point3d$origin, xDirection: _opensolid$geometry$OpenSolid_Direction3d$x, yDirection: _opensolid$geometry$OpenSolid_Direction3d$y});

var _opensolid$geometry$OpenSolid_Circle2d$radius = function (_p0) {
	var _p1 = _p0;
	return _p1._0.radius;
};
var _opensolid$geometry$OpenSolid_Circle2d$diameter = function (circle) {
	return 2 * _opensolid$geometry$OpenSolid_Circle2d$radius(circle);
};
var _opensolid$geometry$OpenSolid_Circle2d$area = function (circle) {
	var r = _opensolid$geometry$OpenSolid_Circle2d$radius(circle);
	return (_elm_lang$core$Basics$pi * r) * r;
};
var _opensolid$geometry$OpenSolid_Circle2d$circumference = function (circle) {
	return (2 * _elm_lang$core$Basics$pi) * _opensolid$geometry$OpenSolid_Circle2d$radius(circle);
};
var _opensolid$geometry$OpenSolid_Circle2d$centerPoint = function (_p2) {
	var _p3 = _p2;
	return _p3._0.centerPoint;
};
var _opensolid$geometry$OpenSolid_Circle2d$contains = F2(
	function (point, circle) {
		var r = _opensolid$geometry$OpenSolid_Circle2d$radius(circle);
		return _elm_lang$core$Native_Utils.cmp(
			A2(
				_opensolid$geometry$OpenSolid_Point2d$squaredDistanceFrom,
				_opensolid$geometry$OpenSolid_Circle2d$centerPoint(circle),
				point),
			r * r) < 1;
	});
var _opensolid$geometry$OpenSolid_Circle2d$scaleAbout = F2(
	function (point, scale) {
		var scalePoint = A2(_opensolid$geometry$OpenSolid_Point2d$scaleAbout, point, scale);
		return function (circle) {
			return _opensolid$geometry$OpenSolid_Geometry_Types$Circle2d(
				{
					centerPoint: scalePoint(
						_opensolid$geometry$OpenSolid_Circle2d$centerPoint(circle)),
					radius: scale * _opensolid$geometry$OpenSolid_Circle2d$radius(circle)
				});
		};
	});
var _opensolid$geometry$OpenSolid_Circle2d$rotateAround = F2(
	function (point, angle) {
		var rotatePoint = A2(_opensolid$geometry$OpenSolid_Point2d$rotateAround, point, angle);
		return function (circle) {
			return _opensolid$geometry$OpenSolid_Geometry_Types$Circle2d(
				{
					centerPoint: rotatePoint(
						_opensolid$geometry$OpenSolid_Circle2d$centerPoint(circle)),
					radius: _opensolid$geometry$OpenSolid_Circle2d$radius(circle)
				});
		};
	});
var _opensolid$geometry$OpenSolid_Circle2d$translateBy = function (displacement) {
	var translatePoint = _opensolid$geometry$OpenSolid_Point2d$translateBy(displacement);
	return function (circle) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Circle2d(
			{
				centerPoint: translatePoint(
					_opensolid$geometry$OpenSolid_Circle2d$centerPoint(circle)),
				radius: _opensolid$geometry$OpenSolid_Circle2d$radius(circle)
			});
	};
};
var _opensolid$geometry$OpenSolid_Circle2d$mirrorAcross = function (axis) {
	var mirrorPoint = _opensolid$geometry$OpenSolid_Point2d$mirrorAcross(axis);
	return function (circle) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Circle2d(
			{
				centerPoint: mirrorPoint(
					_opensolid$geometry$OpenSolid_Circle2d$centerPoint(circle)),
				radius: _opensolid$geometry$OpenSolid_Circle2d$radius(circle)
			});
	};
};
var _opensolid$geometry$OpenSolid_Circle2d$relativeTo = function (frame) {
	var relativePoint = _opensolid$geometry$OpenSolid_Point2d$relativeTo(frame);
	return function (circle) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Circle2d(
			{
				centerPoint: relativePoint(
					_opensolid$geometry$OpenSolid_Circle2d$centerPoint(circle)),
				radius: _opensolid$geometry$OpenSolid_Circle2d$radius(circle)
			});
	};
};
var _opensolid$geometry$OpenSolid_Circle2d$placeIn = function (frame) {
	var placePoint = _opensolid$geometry$OpenSolid_Point2d$placeIn(frame);
	return function (circle) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Circle2d(
			{
				centerPoint: placePoint(
					_opensolid$geometry$OpenSolid_Circle2d$centerPoint(circle)),
				radius: _opensolid$geometry$OpenSolid_Circle2d$radius(circle)
			});
	};
};
var _opensolid$geometry$OpenSolid_Circle2d$placeOnto = F2(
	function (sketchPlane, circle) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Circle3d(
			{
				centerPoint: A2(
					_opensolid$geometry$OpenSolid_Point2d$placeOnto,
					sketchPlane,
					_opensolid$geometry$OpenSolid_Circle2d$centerPoint(circle)),
				axialDirection: _opensolid$geometry$OpenSolid_SketchPlane3d$normalDirection(sketchPlane),
				radius: _opensolid$geometry$OpenSolid_Circle2d$radius(circle)
			});
	});
var _opensolid$geometry$OpenSolid_Circle2d$boundingBox = function (circle) {
	var r = _opensolid$geometry$OpenSolid_Circle2d$radius(circle);
	var _p4 = _opensolid$geometry$OpenSolid_Point2d$coordinates(
		_opensolid$geometry$OpenSolid_Circle2d$centerPoint(circle));
	var x = _p4._0;
	var y = _p4._1;
	return _opensolid$geometry$OpenSolid_Geometry_Types$BoundingBox2d(
		{minX: x - r, maxX: x + r, minY: y - r, maxY: y + r});
};
var _opensolid$geometry$OpenSolid_Circle2d$throughPoints = F3(
	function (firstPoint, secondPoint, thirdPoint) {
		var c2 = A2(_opensolid$geometry$OpenSolid_Point2d$squaredDistanceFrom, thirdPoint, firstPoint);
		var b2 = A2(_opensolid$geometry$OpenSolid_Point2d$squaredDistanceFrom, secondPoint, thirdPoint);
		var a2 = A2(_opensolid$geometry$OpenSolid_Point2d$squaredDistanceFrom, firstPoint, secondPoint);
		var t1 = a2 * ((b2 + c2) - a2);
		var t2 = b2 * ((c2 + a2) - b2);
		var t3 = c2 * ((a2 + b2) - c2);
		var sum = (t1 + t2) + t3;
		if (_elm_lang$core$Native_Utils.eq(sum, 0)) {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			var _p5 = _opensolid$geometry$OpenSolid_Point2d$coordinates(thirdPoint);
			var x3 = _p5._0;
			var y3 = _p5._1;
			var _p6 = _opensolid$geometry$OpenSolid_Point2d$coordinates(secondPoint);
			var x2 = _p6._0;
			var y2 = _p6._1;
			var _p7 = _opensolid$geometry$OpenSolid_Point2d$coordinates(firstPoint);
			var x1 = _p7._0;
			var y1 = _p7._1;
			var w3 = t3 / sum;
			var w2 = t2 / sum;
			var w1 = t1 / sum;
			var centerPoint = _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
				{ctor: '_Tuple2', _0: ((w1 * x3) + (w2 * x1)) + (w3 * x2), _1: ((w1 * y3) + (w2 * y1)) + (w3 * y2)});
			var r1 = A2(_opensolid$geometry$OpenSolid_Point2d$distanceFrom, centerPoint, firstPoint);
			var r2 = A2(_opensolid$geometry$OpenSolid_Point2d$distanceFrom, centerPoint, secondPoint);
			var r3 = A2(_opensolid$geometry$OpenSolid_Point2d$distanceFrom, centerPoint, thirdPoint);
			return _elm_lang$core$Maybe$Just(
				_opensolid$geometry$OpenSolid_Geometry_Types$Circle2d(
					{centerPoint: centerPoint, radius: ((r1 + r2) + r3) / 3}));
		}
	});
var _opensolid$geometry$OpenSolid_Circle2d$unit = _opensolid$geometry$OpenSolid_Geometry_Types$Circle2d(
	{centerPoint: _opensolid$geometry$OpenSolid_Point2d$origin, radius: 1});

var _opensolid$geometry$OpenSolid_Arc2d$sweptAngle = function (_p0) {
	var _p1 = _p0;
	return _p1._0.sweptAngle;
};
var _opensolid$geometry$OpenSolid_Arc2d$startPoint = function (_p2) {
	var _p3 = _p2;
	return _p3._0.startPoint;
};
var _opensolid$geometry$OpenSolid_Arc2d$centerPoint = function (_p4) {
	var _p5 = _p4;
	return _p5._0.centerPoint;
};
var _opensolid$geometry$OpenSolid_Arc2d$radius = function (arc) {
	return A2(
		_opensolid$geometry$OpenSolid_Point2d$distanceFrom,
		_opensolid$geometry$OpenSolid_Arc2d$centerPoint(arc),
		_opensolid$geometry$OpenSolid_Arc2d$startPoint(arc));
};
var _opensolid$geometry$OpenSolid_Arc2d$endPoint = function (arc) {
	return A3(
		_opensolid$geometry$OpenSolid_Point2d$rotateAround,
		_opensolid$geometry$OpenSolid_Arc2d$centerPoint(arc),
		_opensolid$geometry$OpenSolid_Arc2d$sweptAngle(arc),
		_opensolid$geometry$OpenSolid_Arc2d$startPoint(arc));
};
var _opensolid$geometry$OpenSolid_Arc2d$point = F2(
	function (arc, parameter) {
		var angle = parameter * _opensolid$geometry$OpenSolid_Arc2d$sweptAngle(arc);
		return A3(
			_opensolid$geometry$OpenSolid_Point2d$rotateAround,
			_opensolid$geometry$OpenSolid_Arc2d$centerPoint(arc),
			angle,
			_opensolid$geometry$OpenSolid_Arc2d$startPoint(arc));
	});
var _opensolid$geometry$OpenSolid_Arc2d$scaleAbout = F3(
	function (point, scale, arc) {
		var scalePoint = A2(_opensolid$geometry$OpenSolid_Point2d$scaleAbout, point, scale);
		return _opensolid$geometry$OpenSolid_Geometry_Types$Arc2d(
			{
				centerPoint: scalePoint(
					_opensolid$geometry$OpenSolid_Arc2d$centerPoint(arc)),
				startPoint: scalePoint(
					_opensolid$geometry$OpenSolid_Arc2d$startPoint(arc)),
				sweptAngle: (_elm_lang$core$Native_Utils.cmp(scale, 0) > 0) ? _opensolid$geometry$OpenSolid_Arc2d$sweptAngle(arc) : (0 - _opensolid$geometry$OpenSolid_Arc2d$sweptAngle(arc))
			});
	});
var _opensolid$geometry$OpenSolid_Arc2d$rotateAround = F2(
	function (point, angle) {
		var rotatePoint = A2(_opensolid$geometry$OpenSolid_Point2d$rotateAround, point, angle);
		return function (arc) {
			return _opensolid$geometry$OpenSolid_Geometry_Types$Arc2d(
				{
					centerPoint: rotatePoint(
						_opensolid$geometry$OpenSolid_Arc2d$centerPoint(arc)),
					startPoint: rotatePoint(
						_opensolid$geometry$OpenSolid_Arc2d$startPoint(arc)),
					sweptAngle: _opensolid$geometry$OpenSolid_Arc2d$sweptAngle(arc)
				});
		};
	});
var _opensolid$geometry$OpenSolid_Arc2d$translateBy = F2(
	function (displacement, arc) {
		var translatePoint = _opensolid$geometry$OpenSolid_Point2d$translateBy(displacement);
		return _opensolid$geometry$OpenSolid_Geometry_Types$Arc2d(
			{
				centerPoint: translatePoint(
					_opensolid$geometry$OpenSolid_Arc2d$centerPoint(arc)),
				startPoint: translatePoint(
					_opensolid$geometry$OpenSolid_Arc2d$startPoint(arc)),
				sweptAngle: _opensolid$geometry$OpenSolid_Arc2d$sweptAngle(arc)
			});
	});
var _opensolid$geometry$OpenSolid_Arc2d$mirrorAcross = function (axis) {
	var mirrorPoint = _opensolid$geometry$OpenSolid_Point2d$mirrorAcross(axis);
	return function (arc) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Arc2d(
			{
				centerPoint: mirrorPoint(
					_opensolid$geometry$OpenSolid_Arc2d$centerPoint(arc)),
				startPoint: mirrorPoint(
					_opensolid$geometry$OpenSolid_Arc2d$startPoint(arc)),
				sweptAngle: 0 - _opensolid$geometry$OpenSolid_Arc2d$sweptAngle(arc)
			});
	};
};
var _opensolid$geometry$OpenSolid_Arc2d$relativeTo = F2(
	function (frame, arc) {
		var relativePoint = _opensolid$geometry$OpenSolid_Point2d$relativeTo(frame);
		return _opensolid$geometry$OpenSolid_Geometry_Types$Arc2d(
			{
				centerPoint: relativePoint(
					_opensolid$geometry$OpenSolid_Arc2d$centerPoint(arc)),
				startPoint: relativePoint(
					_opensolid$geometry$OpenSolid_Arc2d$startPoint(arc)),
				sweptAngle: _opensolid$geometry$OpenSolid_Frame2d$isRightHanded(frame) ? _opensolid$geometry$OpenSolid_Arc2d$sweptAngle(arc) : (0 - _opensolid$geometry$OpenSolid_Arc2d$sweptAngle(arc))
			});
	});
var _opensolid$geometry$OpenSolid_Arc2d$placeIn = F2(
	function (frame, arc) {
		var placePoint = _opensolid$geometry$OpenSolid_Point2d$placeIn(frame);
		return _opensolid$geometry$OpenSolid_Geometry_Types$Arc2d(
			{
				centerPoint: placePoint(
					_opensolid$geometry$OpenSolid_Arc2d$centerPoint(arc)),
				startPoint: placePoint(
					_opensolid$geometry$OpenSolid_Arc2d$startPoint(arc)),
				sweptAngle: _opensolid$geometry$OpenSolid_Frame2d$isRightHanded(frame) ? _opensolid$geometry$OpenSolid_Arc2d$sweptAngle(arc) : (0 - _opensolid$geometry$OpenSolid_Arc2d$sweptAngle(arc))
			});
	});
var _opensolid$geometry$OpenSolid_Arc2d$placeOnto = F2(
	function (sketchPlane, arc) {
		var place = _opensolid$geometry$OpenSolid_Point2d$placeOnto(sketchPlane);
		var axis = _opensolid$geometry$OpenSolid_Geometry_Types$Axis3d(
			{
				originPoint: place(
					_opensolid$geometry$OpenSolid_Arc2d$centerPoint(arc)),
				direction: _opensolid$geometry$OpenSolid_SketchPlane3d$normalDirection(sketchPlane)
			});
		return _opensolid$geometry$OpenSolid_Geometry_Types$Arc3d(
			{
				axis: axis,
				startPoint: place(
					_opensolid$geometry$OpenSolid_Arc2d$startPoint(arc)),
				sweptAngle: _opensolid$geometry$OpenSolid_Arc2d$sweptAngle(arc)
			});
	});
var _opensolid$geometry$OpenSolid_Arc2d$fromEndpoints = F5(
	function (startPoint, endPoint, radius, lengthType, windingDirection) {
		var squaredRadius = radius * radius;
		var chord = _opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d(
			{ctor: '_Tuple2', _0: startPoint, _1: endPoint});
		var squaredHalfLength = _opensolid$geometry$OpenSolid_LineSegment2d$squaredLength(chord) / 4;
		return (_elm_lang$core$Native_Utils.cmp(squaredRadius, squaredHalfLength) > -1) ? A2(
			_elm_lang$core$Maybe$map,
			function (offsetDirection) {
				var halfLength = _elm_lang$core$Basics$sqrt(squaredHalfLength);
				var shortAngle = 2 * _elm_lang$core$Basics$asin(halfLength / radius);
				var sweptAngle = function () {
					var _p6 = {ctor: '_Tuple2', _0: windingDirection, _1: lengthType};
					if (_p6._0.ctor === 'Counterclockwise') {
						if (_p6._1.ctor === 'Short') {
							return shortAngle;
						} else {
							return (2 * _elm_lang$core$Basics$pi) - shortAngle;
						}
					} else {
						if (_p6._1.ctor === 'Short') {
							return 0 - shortAngle;
						} else {
							return shortAngle - (2 * _elm_lang$core$Basics$pi);
						}
					}
				}();
				var midpoint = _opensolid$geometry$OpenSolid_LineSegment2d$midpoint(chord);
				var offsetMagnitude = _elm_lang$core$Basics$sqrt(squaredRadius - squaredHalfLength);
				var offsetDistance = function () {
					var _p7 = {ctor: '_Tuple2', _0: windingDirection, _1: lengthType};
					if (_p7._0.ctor === 'Clockwise') {
						if (_p7._1.ctor === 'Long') {
							return offsetMagnitude;
						} else {
							return 0 - offsetMagnitude;
						}
					} else {
						if (_p7._1.ctor === 'Short') {
							return offsetMagnitude;
						} else {
							return 0 - offsetMagnitude;
						}
					}
				}();
				var offset = A2(_opensolid$geometry$OpenSolid_Vector2d$in_, offsetDirection, offsetDistance);
				var centerPoint = A2(_opensolid$geometry$OpenSolid_Point2d$translateBy, offset, midpoint);
				return _opensolid$geometry$OpenSolid_Geometry_Types$Arc2d(
					{centerPoint: centerPoint, startPoint: startPoint, sweptAngle: sweptAngle});
			},
			_opensolid$geometry$OpenSolid_LineSegment2d$normalDirection(chord)) : _elm_lang$core$Maybe$Nothing;
	});
var _opensolid$geometry$OpenSolid_Arc2d$throughPoints = F3(
	function (firstPoint, secondPoint, thirdPoint) {
		return A2(
			_elm_lang$core$Maybe$andThen,
			function (circle) {
				var centerPoint = _opensolid$geometry$OpenSolid_Circle2d$centerPoint(circle);
				var firstVector = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, centerPoint, firstPoint);
				var secondVector = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, centerPoint, secondPoint);
				var thirdVector = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, centerPoint, thirdPoint);
				return A4(
					_elm_lang$core$Maybe$map3,
					F3(
						function (firstDirection, secondDirection, thirdDirection) {
							var full = A2(_opensolid$geometry$OpenSolid_Direction2d$angleFrom, firstDirection, thirdDirection);
							var partial = A2(_opensolid$geometry$OpenSolid_Direction2d$angleFrom, firstDirection, secondDirection);
							var sweptAngle = ((_elm_lang$core$Native_Utils.cmp(partial, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(full, partial) > -1)) ? full : (((_elm_lang$core$Native_Utils.cmp(partial, 0) < 1) && (_elm_lang$core$Native_Utils.cmp(full, partial) < 1)) ? full : ((_elm_lang$core$Native_Utils.cmp(full, 0) > -1) ? (full - (2 * _elm_lang$core$Basics$pi)) : (full + (2 * _elm_lang$core$Basics$pi))));
							return _opensolid$geometry$OpenSolid_Geometry_Types$Arc2d(
								{centerPoint: centerPoint, startPoint: firstPoint, sweptAngle: sweptAngle});
						}),
					_opensolid$geometry$OpenSolid_Vector2d$direction(firstVector),
					_opensolid$geometry$OpenSolid_Vector2d$direction(secondVector),
					_opensolid$geometry$OpenSolid_Vector2d$direction(thirdVector));
			},
			A3(_opensolid$geometry$OpenSolid_Circle2d$throughPoints, firstPoint, secondPoint, thirdPoint));
	});
var _opensolid$geometry$OpenSolid_Arc2d$Long = {ctor: 'Long'};
var _opensolid$geometry$OpenSolid_Arc2d$long = _opensolid$geometry$OpenSolid_Arc2d$Long;
var _opensolid$geometry$OpenSolid_Arc2d$Short = {ctor: 'Short'};
var _opensolid$geometry$OpenSolid_Arc2d$short = _opensolid$geometry$OpenSolid_Arc2d$Short;
var _opensolid$geometry$OpenSolid_Arc2d$Counterclockwise = {ctor: 'Counterclockwise'};
var _opensolid$geometry$OpenSolid_Arc2d$counterclockwise = _opensolid$geometry$OpenSolid_Arc2d$Counterclockwise;
var _opensolid$geometry$OpenSolid_Arc2d$Clockwise = {ctor: 'Clockwise'};
var _opensolid$geometry$OpenSolid_Arc2d$clockwise = _opensolid$geometry$OpenSolid_Arc2d$Clockwise;

var _opensolid$geometry$OpenSolid_QuadraticSpline2d$endPoint = function (_p0) {
	var _p1 = _p0;
	return _p1._0._2;
};
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$startPoint = function (_p2) {
	var _p3 = _p2;
	return _p3._0._0;
};
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$controlPoints = function (_p4) {
	var _p5 = _p4;
	return _p5._0;
};
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$startDerivative = function (spline) {
	var _p6 = _opensolid$geometry$OpenSolid_QuadraticSpline2d$controlPoints(spline);
	var p1 = _p6._0;
	var p2 = _p6._1;
	return A2(
		_opensolid$geometry$OpenSolid_Vector2d$scaleBy,
		2,
		A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p1, p2));
};
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$endDerivative = function (spline) {
	var _p7 = _opensolid$geometry$OpenSolid_QuadraticSpline2d$controlPoints(spline);
	var p2 = _p7._1;
	var p3 = _p7._2;
	return A2(
		_opensolid$geometry$OpenSolid_Vector2d$scaleBy,
		2,
		A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p2, p3));
};
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$point = F2(
	function (spline, t) {
		var _p8 = _opensolid$geometry$OpenSolid_QuadraticSpline2d$controlPoints(spline);
		var p1 = _p8._0;
		var p2 = _p8._1;
		var p3 = _p8._2;
		var q1 = A3(_opensolid$geometry$OpenSolid_Point2d$interpolateFrom, p1, p2, t);
		var q2 = A3(_opensolid$geometry$OpenSolid_Point2d$interpolateFrom, p2, p3, t);
		return A3(_opensolid$geometry$OpenSolid_Point2d$interpolateFrom, q1, q2, t);
	});
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$derivative = function (spline) {
	var _p9 = _opensolid$geometry$OpenSolid_QuadraticSpline2d$controlPoints(spline);
	var p1 = _p9._0;
	var p2 = _p9._1;
	var p3 = _p9._2;
	var v1 = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p1, p2);
	var v2 = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p2, p3);
	return function (t) {
		return A2(
			_opensolid$geometry$OpenSolid_Vector2d$scaleBy,
			2,
			A3(_opensolid$geometry$OpenSolid_Vector2d$interpolateFrom, v1, v2, t));
	};
};
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$mapControlPoints = F2(
	function ($function, spline) {
		var _p10 = _opensolid$geometry$OpenSolid_QuadraticSpline2d$controlPoints(spline);
		var p1 = _p10._0;
		var p2 = _p10._1;
		var p3 = _p10._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$QuadraticSpline2d(
			{
				ctor: '_Tuple3',
				_0: $function(p1),
				_1: $function(p2),
				_2: $function(p3)
			});
	});
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$scaleAbout = F2(
	function (point, scale) {
		return _opensolid$geometry$OpenSolid_QuadraticSpline2d$mapControlPoints(
			A2(_opensolid$geometry$OpenSolid_Point2d$scaleAbout, point, scale));
	});
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$rotateAround = F2(
	function (point, angle) {
		return _opensolid$geometry$OpenSolid_QuadraticSpline2d$mapControlPoints(
			A2(_opensolid$geometry$OpenSolid_Point2d$rotateAround, point, angle));
	});
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$translateBy = function (displacement) {
	return _opensolid$geometry$OpenSolid_QuadraticSpline2d$mapControlPoints(
		_opensolid$geometry$OpenSolid_Point2d$translateBy(displacement));
};
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$mirrorAcross = function (axis) {
	return _opensolid$geometry$OpenSolid_QuadraticSpline2d$mapControlPoints(
		_opensolid$geometry$OpenSolid_Point2d$mirrorAcross(axis));
};
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$relativeTo = function (frame) {
	return _opensolid$geometry$OpenSolid_QuadraticSpline2d$mapControlPoints(
		_opensolid$geometry$OpenSolid_Point2d$relativeTo(frame));
};
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$placeIn = function (frame) {
	return _opensolid$geometry$OpenSolid_QuadraticSpline2d$mapControlPoints(
		_opensolid$geometry$OpenSolid_Point2d$placeIn(frame));
};
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$placeOnto = F2(
	function (sketchPlane, spline) {
		var place = _opensolid$geometry$OpenSolid_Point2d$placeOnto(sketchPlane);
		var _p11 = _opensolid$geometry$OpenSolid_QuadraticSpline2d$controlPoints(spline);
		var p1 = _p11._0;
		var p2 = _p11._1;
		var p3 = _p11._2;
		return _opensolid$geometry$OpenSolid_Geometry_Types$QuadraticSpline3d(
			{
				ctor: '_Tuple3',
				_0: place(p1),
				_1: place(p2),
				_2: place(p3)
			});
	});
var _opensolid$geometry$OpenSolid_QuadraticSpline2d$bezier = F3(
	function (firstPoint, secondPoint, thirdPoint) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$QuadraticSpline2d(
			{ctor: '_Tuple3', _0: firstPoint, _1: secondPoint, _2: thirdPoint});
	});

var _opensolid$geometry$OpenSolid_CubicSpline2d$endPoint = function (_p0) {
	var _p1 = _p0;
	return _p1._0._3;
};
var _opensolid$geometry$OpenSolid_CubicSpline2d$startPoint = function (_p2) {
	var _p3 = _p2;
	return _p3._0._0;
};
var _opensolid$geometry$OpenSolid_CubicSpline2d$controlPoints = function (_p4) {
	var _p5 = _p4;
	return _p5._0;
};
var _opensolid$geometry$OpenSolid_CubicSpline2d$startDerivative = function (spline) {
	var _p6 = _opensolid$geometry$OpenSolid_CubicSpline2d$controlPoints(spline);
	var p1 = _p6._0;
	var p2 = _p6._1;
	return A2(
		_opensolid$geometry$OpenSolid_Vector2d$scaleBy,
		3,
		A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p1, p2));
};
var _opensolid$geometry$OpenSolid_CubicSpline2d$endDerivative = function (spline) {
	var _p7 = _opensolid$geometry$OpenSolid_CubicSpline2d$controlPoints(spline);
	var p3 = _p7._2;
	var p4 = _p7._3;
	return A2(
		_opensolid$geometry$OpenSolid_Vector2d$scaleBy,
		3,
		A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p3, p4));
};
var _opensolid$geometry$OpenSolid_CubicSpline2d$point = F2(
	function (spline, t) {
		var _p8 = _opensolid$geometry$OpenSolid_CubicSpline2d$controlPoints(spline);
		var p1 = _p8._0;
		var p2 = _p8._1;
		var p3 = _p8._2;
		var p4 = _p8._3;
		var q1 = A3(_opensolid$geometry$OpenSolid_Point2d$interpolateFrom, p1, p2, t);
		var q2 = A3(_opensolid$geometry$OpenSolid_Point2d$interpolateFrom, p2, p3, t);
		var r1 = A3(_opensolid$geometry$OpenSolid_Point2d$interpolateFrom, q1, q2, t);
		var q3 = A3(_opensolid$geometry$OpenSolid_Point2d$interpolateFrom, p3, p4, t);
		var r2 = A3(_opensolid$geometry$OpenSolid_Point2d$interpolateFrom, q2, q3, t);
		return A3(_opensolid$geometry$OpenSolid_Point2d$interpolateFrom, r1, r2, t);
	});
var _opensolid$geometry$OpenSolid_CubicSpline2d$derivative = function (spline) {
	var _p9 = _opensolid$geometry$OpenSolid_CubicSpline2d$controlPoints(spline);
	var p1 = _p9._0;
	var p2 = _p9._1;
	var p3 = _p9._2;
	var p4 = _p9._3;
	var v1 = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p1, p2);
	var v2 = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p2, p3);
	var v3 = A2(_opensolid$geometry$OpenSolid_Point2d$vectorFrom, p3, p4);
	return function (t) {
		var w2 = A3(_opensolid$geometry$OpenSolid_Vector2d$interpolateFrom, v2, v3, t);
		var w1 = A3(_opensolid$geometry$OpenSolid_Vector2d$interpolateFrom, v1, v2, t);
		return A2(
			_opensolid$geometry$OpenSolid_Vector2d$scaleBy,
			3,
			A3(_opensolid$geometry$OpenSolid_Vector2d$interpolateFrom, w1, w2, t));
	};
};
var _opensolid$geometry$OpenSolid_CubicSpline2d$mapControlPoints = F2(
	function ($function, spline) {
		var _p10 = _opensolid$geometry$OpenSolid_CubicSpline2d$controlPoints(spline);
		var p1 = _p10._0;
		var p2 = _p10._1;
		var p3 = _p10._2;
		var p4 = _p10._3;
		return _opensolid$geometry$OpenSolid_Geometry_Types$CubicSpline2d(
			{
				ctor: '_Tuple4',
				_0: $function(p1),
				_1: $function(p2),
				_2: $function(p3),
				_3: $function(p4)
			});
	});
var _opensolid$geometry$OpenSolid_CubicSpline2d$scaleAbout = F2(
	function (point, scale) {
		return _opensolid$geometry$OpenSolid_CubicSpline2d$mapControlPoints(
			A2(_opensolid$geometry$OpenSolid_Point2d$scaleAbout, point, scale));
	});
var _opensolid$geometry$OpenSolid_CubicSpline2d$rotateAround = F2(
	function (point, angle) {
		return _opensolid$geometry$OpenSolid_CubicSpline2d$mapControlPoints(
			A2(_opensolid$geometry$OpenSolid_Point2d$rotateAround, point, angle));
	});
var _opensolid$geometry$OpenSolid_CubicSpline2d$translateBy = function (displacement) {
	return _opensolid$geometry$OpenSolid_CubicSpline2d$mapControlPoints(
		_opensolid$geometry$OpenSolid_Point2d$translateBy(displacement));
};
var _opensolid$geometry$OpenSolid_CubicSpline2d$mirrorAcross = function (axis) {
	return _opensolid$geometry$OpenSolid_CubicSpline2d$mapControlPoints(
		_opensolid$geometry$OpenSolid_Point2d$mirrorAcross(axis));
};
var _opensolid$geometry$OpenSolid_CubicSpline2d$relativeTo = function (frame) {
	return _opensolid$geometry$OpenSolid_CubicSpline2d$mapControlPoints(
		_opensolid$geometry$OpenSolid_Point2d$relativeTo(frame));
};
var _opensolid$geometry$OpenSolid_CubicSpline2d$placeIn = function (frame) {
	return _opensolid$geometry$OpenSolid_CubicSpline2d$mapControlPoints(
		_opensolid$geometry$OpenSolid_Point2d$placeIn(frame));
};
var _opensolid$geometry$OpenSolid_CubicSpline2d$placeOnto = F2(
	function (sketchPlane, spline) {
		var place = _opensolid$geometry$OpenSolid_Point2d$placeOnto(sketchPlane);
		var _p11 = _opensolid$geometry$OpenSolid_CubicSpline2d$controlPoints(spline);
		var p1 = _p11._0;
		var p2 = _p11._1;
		var p3 = _p11._2;
		var p4 = _p11._3;
		return _opensolid$geometry$OpenSolid_Geometry_Types$CubicSpline3d(
			{
				ctor: '_Tuple4',
				_0: place(p1),
				_1: place(p2),
				_2: place(p3),
				_3: place(p4)
			});
	});
var _opensolid$geometry$OpenSolid_CubicSpline2d$bezier = F4(
	function (firstPoint, secondPoint, thirdPoint, fourthPoint) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$CubicSpline2d(
			{ctor: '_Tuple4', _0: firstPoint, _1: secondPoint, _2: thirdPoint, _3: fourthPoint});
	});
var _opensolid$geometry$OpenSolid_CubicSpline2d$hermite = F2(
	function (_p13, _p12) {
		var _p14 = _p13;
		var _p17 = _p14._0;
		var _p15 = _p12;
		var _p16 = _p15._0;
		var endControlPoint = A2(
			_opensolid$geometry$OpenSolid_Point2d$translateBy,
			A2(_opensolid$geometry$OpenSolid_Vector2d$scaleBy, -1 / 3, _p15._1),
			_p16);
		var startControlPoint = A2(
			_opensolid$geometry$OpenSolid_Point2d$translateBy,
			A2(_opensolid$geometry$OpenSolid_Vector2d$scaleBy, 1 / 3, _p14._1),
			_p17);
		return A4(_opensolid$geometry$OpenSolid_CubicSpline2d$bezier, _p17, startControlPoint, endControlPoint, _p16);
	});

var _opensolid$svg$OpenSolid_Svg$placeIn = F2(
	function (frame, element) {
		var _p0 = _opensolid$geometry$OpenSolid_Direction2d$components(
			_opensolid$geometry$OpenSolid_Frame2d$yDirection(frame));
		var x2 = _p0._0;
		var y2 = _p0._1;
		var _p1 = _opensolid$geometry$OpenSolid_Direction2d$components(
			_opensolid$geometry$OpenSolid_Frame2d$xDirection(frame));
		var x1 = _p1._0;
		var y1 = _p1._1;
		var _p2 = _opensolid$geometry$OpenSolid_Point2d$coordinates(
			_opensolid$geometry$OpenSolid_Frame2d$originPoint(frame));
		var px = _p2._0;
		var py = _p2._1;
		var components = A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Basics$toString,
			{
				ctor: '::',
				_0: x1,
				_1: {
					ctor: '::',
					_0: y1,
					_1: {
						ctor: '::',
						_0: x2,
						_1: {
							ctor: '::',
							_0: y2,
							_1: {
								ctor: '::',
								_0: px,
								_1: {
									ctor: '::',
									_0: py,
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			});
		var transform = A2(
			_elm_lang$core$Basics_ops['++'],
			'matrix(',
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(_elm_lang$core$String$join, ' ', components),
				')'));
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$transform(transform),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: element,
				_1: {ctor: '[]'}
			});
	});
var _opensolid$svg$OpenSolid_Svg$relativeTo = function (frame) {
	return _opensolid$svg$OpenSolid_Svg$placeIn(
		A2(_opensolid$geometry$OpenSolid_Frame2d$relativeTo, frame, _opensolid$geometry$OpenSolid_Frame2d$xy));
};
var _opensolid$svg$OpenSolid_Svg$mirrorAcross = function (axis) {
	return _opensolid$svg$OpenSolid_Svg$placeIn(
		A2(_opensolid$geometry$OpenSolid_Frame2d$mirrorAcross, axis, _opensolid$geometry$OpenSolid_Frame2d$xy));
};
var _opensolid$svg$OpenSolid_Svg$translateBy = function (vector) {
	return _opensolid$svg$OpenSolid_Svg$placeIn(
		A2(_opensolid$geometry$OpenSolid_Frame2d$translateBy, vector, _opensolid$geometry$OpenSolid_Frame2d$xy));
};
var _opensolid$svg$OpenSolid_Svg$rotateAround = F2(
	function (point, angle) {
		return _opensolid$svg$OpenSolid_Svg$placeIn(
			A3(_opensolid$geometry$OpenSolid_Frame2d$rotateAround, point, angle, _opensolid$geometry$OpenSolid_Frame2d$xy));
	});
var _opensolid$svg$OpenSolid_Svg$scaleAbout = F3(
	function (point, scale, element) {
		var _p3 = _opensolid$geometry$OpenSolid_Point2d$coordinates(
			A3(_opensolid$geometry$OpenSolid_Point2d$scaleAbout, point, scale, _opensolid$geometry$OpenSolid_Point2d$origin));
		var px = _p3._0;
		var py = _p3._1;
		var components = A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Basics$toString,
			{
				ctor: '::',
				_0: scale,
				_1: {
					ctor: '::',
					_0: 0,
					_1: {
						ctor: '::',
						_0: 0,
						_1: {
							ctor: '::',
							_0: scale,
							_1: {
								ctor: '::',
								_0: px,
								_1: {
									ctor: '::',
									_0: py,
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			});
		var transform = A2(
			_elm_lang$core$Basics_ops['++'],
			'matrix(',
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(_elm_lang$core$String$join, ' ', components),
				')'));
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$transform(transform),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: element,
				_1: {ctor: '[]'}
			});
	});
var _opensolid$svg$OpenSolid_Svg$text2d = F3(
	function (attributes, basePoint, text) {
		var mirrorAxis = _opensolid$geometry$OpenSolid_Geometry_Types$Axis2d(
			{originPoint: basePoint, direction: _opensolid$geometry$OpenSolid_Direction2d$x});
		var _p4 = _opensolid$geometry$OpenSolid_Point2d$coordinates(basePoint);
		var x = _p4._0;
		var y = _p4._1;
		var xAttribute = _elm_lang$svg$Svg_Attributes$x(
			_elm_lang$core$Basics$toString(x));
		var yAttribute = _elm_lang$svg$Svg_Attributes$y(
			_elm_lang$core$Basics$toString(y));
		return A2(
			_opensolid$svg$OpenSolid_Svg$mirrorAcross,
			mirrorAxis,
			A2(
				_elm_lang$svg$Svg$text_,
				{
					ctor: '::',
					_0: xAttribute,
					_1: {ctor: '::', _0: yAttribute, _1: attributes}
				},
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg$text(text),
					_1: {ctor: '[]'}
				}));
	});
var _opensolid$svg$OpenSolid_Svg$cubicSpline2d = F2(
	function (attributes, spline) {
		var _p5 = _opensolid$geometry$OpenSolid_CubicSpline2d$controlPoints(spline);
		var p1 = _p5._0;
		var p2 = _p5._1;
		var p3 = _p5._2;
		var p4 = _p5._3;
		var _p6 = _opensolid$geometry$OpenSolid_Point2d$coordinates(p1);
		var x1 = _p6._0;
		var y1 = _p6._1;
		var _p7 = _opensolid$geometry$OpenSolid_Point2d$coordinates(p2);
		var x2 = _p7._0;
		var y2 = _p7._1;
		var _p8 = _opensolid$geometry$OpenSolid_Point2d$coordinates(p3);
		var x3 = _p8._0;
		var y3 = _p8._1;
		var _p9 = _opensolid$geometry$OpenSolid_Point2d$coordinates(p4);
		var x4 = _p9._0;
		var y4 = _p9._1;
		var pathComponents = {
			ctor: '::',
			_0: 'M',
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Basics$toString(x1),
				_1: {
					ctor: '::',
					_0: _elm_lang$core$Basics$toString(y1),
					_1: {
						ctor: '::',
						_0: 'C',
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Basics$toString(x2),
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(y2),
								_1: {
									ctor: '::',
									_0: _elm_lang$core$Basics$toString(x3),
									_1: {
										ctor: '::',
										_0: _elm_lang$core$Basics$toString(y3),
										_1: {
											ctor: '::',
											_0: _elm_lang$core$Basics$toString(x4),
											_1: {
												ctor: '::',
												_0: _elm_lang$core$Basics$toString(y4),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		};
		var pathAttribute = _elm_lang$svg$Svg_Attributes$d(
			A2(_elm_lang$core$String$join, ' ', pathComponents));
		return A2(
			_elm_lang$svg$Svg$path,
			{ctor: '::', _0: pathAttribute, _1: attributes},
			{ctor: '[]'});
	});
var _opensolid$svg$OpenSolid_Svg$quadraticSpline2d = F2(
	function (attributes, spline) {
		var _p10 = _opensolid$geometry$OpenSolid_QuadraticSpline2d$controlPoints(spline);
		var p1 = _p10._0;
		var p2 = _p10._1;
		var p3 = _p10._2;
		var _p11 = _opensolid$geometry$OpenSolid_Point2d$coordinates(p1);
		var x1 = _p11._0;
		var y1 = _p11._1;
		var _p12 = _opensolid$geometry$OpenSolid_Point2d$coordinates(p2);
		var x2 = _p12._0;
		var y2 = _p12._1;
		var _p13 = _opensolid$geometry$OpenSolid_Point2d$coordinates(p3);
		var x3 = _p13._0;
		var y3 = _p13._1;
		var pathComponents = {
			ctor: '::',
			_0: 'M',
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Basics$toString(x1),
				_1: {
					ctor: '::',
					_0: _elm_lang$core$Basics$toString(y1),
					_1: {
						ctor: '::',
						_0: 'Q',
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Basics$toString(x2),
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(y2),
								_1: {
									ctor: '::',
									_0: _elm_lang$core$Basics$toString(x3),
									_1: {
										ctor: '::',
										_0: _elm_lang$core$Basics$toString(y3),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}
			}
		};
		var pathAttribute = _elm_lang$svg$Svg_Attributes$d(
			A2(_elm_lang$core$String$join, ' ', pathComponents));
		return A2(
			_elm_lang$svg$Svg$path,
			{ctor: '::', _0: pathAttribute, _1: attributes},
			{ctor: '[]'});
	});
var _opensolid$svg$OpenSolid_Svg$circle2d = F2(
	function (attributes, circle) {
		var r = _elm_lang$svg$Svg_Attributes$r(
			_elm_lang$core$Basics$toString(
				_opensolid$geometry$OpenSolid_Circle2d$radius(circle)));
		var _p14 = _opensolid$geometry$OpenSolid_Point2d$coordinates(
			_opensolid$geometry$OpenSolid_Circle2d$centerPoint(circle));
		var x = _p14._0;
		var y = _p14._1;
		var cx = _elm_lang$svg$Svg_Attributes$cx(
			_elm_lang$core$Basics$toString(x));
		var cy = _elm_lang$svg$Svg_Attributes$cy(
			_elm_lang$core$Basics$toString(y));
		return A2(
			_elm_lang$svg$Svg$circle,
			{
				ctor: '::',
				_0: cx,
				_1: {
					ctor: '::',
					_0: cy,
					_1: {ctor: '::', _0: r, _1: attributes}
				}
			},
			{ctor: '[]'});
	});
var _opensolid$svg$OpenSolid_Svg$arc2d = F2(
	function (attributes, arc) {
		var radius = _opensolid$geometry$OpenSolid_Arc2d$radius(arc);
		var radiusString = _elm_lang$core$Basics$toString(radius);
		var _p15 = _opensolid$geometry$OpenSolid_Point2d$coordinates(
			_opensolid$geometry$OpenSolid_Arc2d$startPoint(arc));
		var x0 = _p15._0;
		var y0 = _p15._1;
		var moveCommand = {
			ctor: '::',
			_0: 'M',
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Basics$toString(x0),
				_1: {
					ctor: '::',
					_0: _elm_lang$core$Basics$toString(y0),
					_1: {ctor: '[]'}
				}
			}
		};
		var maxSegmentAngle = (2 * _elm_lang$core$Basics$pi) / 3;
		var sweptAngle = _opensolid$geometry$OpenSolid_Arc2d$sweptAngle(arc);
		var numSegments = 1 + _elm_lang$core$Basics$floor(
			_elm_lang$core$Basics$abs(sweptAngle) / maxSegmentAngle);
		var sweepFlag = (_elm_lang$core$Native_Utils.cmp(sweptAngle, 0) > -1) ? '1' : '0';
		var arcSegment = function (index) {
			var t = _elm_lang$core$Basics$toFloat(index) / _elm_lang$core$Basics$toFloat(numSegments);
			var _p16 = _opensolid$geometry$OpenSolid_Point2d$coordinates(
				A2(_opensolid$geometry$OpenSolid_Arc2d$point, arc, t));
			var x = _p16._0;
			var y = _p16._1;
			return {
				ctor: '::',
				_0: 'A',
				_1: {
					ctor: '::',
					_0: radiusString,
					_1: {
						ctor: '::',
						_0: radiusString,
						_1: {
							ctor: '::',
							_0: '0',
							_1: {
								ctor: '::',
								_0: '0',
								_1: {
									ctor: '::',
									_0: sweepFlag,
									_1: {
										ctor: '::',
										_0: _elm_lang$core$Basics$toString(x),
										_1: {
											ctor: '::',
											_0: _elm_lang$core$Basics$toString(y),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				}
			};
		};
		var arcSegments = A2(
			_elm_lang$core$List$map,
			arcSegment,
			A2(_elm_lang$core$List$range, 1, numSegments));
		var pathComponents = A2(
			_elm_lang$core$Basics_ops['++'],
			moveCommand,
			_elm_lang$core$List$concat(arcSegments));
		var pathAttribute = _elm_lang$svg$Svg_Attributes$d(
			A2(_elm_lang$core$String$join, ' ', pathComponents));
		return A2(
			_elm_lang$svg$Svg$path,
			{ctor: '::', _0: pathAttribute, _1: attributes},
			{ctor: '[]'});
	});
var _opensolid$svg$OpenSolid_Svg$point2d = F2(
	function (options, point) {
		return A2(
			_opensolid$svg$OpenSolid_Svg$circle2d,
			options.attributes,
			_opensolid$geometry$OpenSolid_Geometry_Types$Circle2d(
				{centerPoint: point, radius: options.radius}));
	});
var _opensolid$svg$OpenSolid_Svg$coordinatesString = function (point) {
	var _p17 = _opensolid$geometry$OpenSolid_Point2d$coordinates(point);
	var x = _p17._0;
	var y = _p17._1;
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(x),
		A2(
			_elm_lang$core$Basics_ops['++'],
			',',
			_elm_lang$core$Basics$toString(y)));
};
var _opensolid$svg$OpenSolid_Svg$pointsAttribute = function (points) {
	return _elm_lang$svg$Svg_Attributes$points(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(_elm_lang$core$List$map, _opensolid$svg$OpenSolid_Svg$coordinatesString, points)));
};
var _opensolid$svg$OpenSolid_Svg$lineSegment2d = F2(
	function (attributes, lineSegment) {
		var _p18 = _opensolid$geometry$OpenSolid_LineSegment2d$endpoints(lineSegment);
		var p1 = _p18._0;
		var p2 = _p18._1;
		return A2(
			_elm_lang$svg$Svg$polyline,
			{
				ctor: '::',
				_0: _opensolid$svg$OpenSolid_Svg$pointsAttribute(
					{
						ctor: '::',
						_0: p1,
						_1: {
							ctor: '::',
							_0: p2,
							_1: {ctor: '[]'}
						}
					}),
				_1: attributes
			},
			{ctor: '[]'});
	});
var _opensolid$svg$OpenSolid_Svg$triangle2d = F2(
	function (attributes, triangle) {
		var _p19 = _opensolid$geometry$OpenSolid_Triangle2d$vertices(triangle);
		var p1 = _p19._0;
		var p2 = _p19._1;
		var p3 = _p19._2;
		return A2(
			_elm_lang$svg$Svg$polygon,
			{
				ctor: '::',
				_0: _opensolid$svg$OpenSolid_Svg$pointsAttribute(
					{
						ctor: '::',
						_0: p1,
						_1: {
							ctor: '::',
							_0: p2,
							_1: {
								ctor: '::',
								_0: p3,
								_1: {ctor: '[]'}
							}
						}
					}),
				_1: attributes
			},
			{ctor: '[]'});
	});
var _opensolid$svg$OpenSolid_Svg$vector2d = F3(
	function (options, basePoint, vector) {
		var _p20 = _opensolid$geometry$OpenSolid_Vector2d$lengthAndDirection(vector);
		if (_p20.ctor === 'Just') {
			var _p22 = _p20._0._0;
			var _p21 = _p20._0._1;
			var tipWidth = options.tipWidth;
			var tipLength = options.tipLength;
			var frame = _opensolid$geometry$OpenSolid_Geometry_Types$Frame2d(
				{
					originPoint: basePoint,
					xDirection: _p21,
					yDirection: _opensolid$geometry$OpenSolid_Direction2d$perpendicularTo(_p21)
				});
			var tipPoint = A2(
				_opensolid$geometry$OpenSolid_Point2d$in_,
				frame,
				{ctor: '_Tuple2', _0: _p22, _1: 0});
			var tipBasePoint = A2(
				_opensolid$geometry$OpenSolid_Point2d$in_,
				frame,
				{ctor: '_Tuple2', _0: _p22 - tipLength, _1: 0});
			var stem = _opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d(
				{ctor: '_Tuple2', _0: basePoint, _1: tipBasePoint});
			var leftPoint = A2(
				_opensolid$geometry$OpenSolid_Point2d$in_,
				frame,
				{ctor: '_Tuple2', _0: _p22 - tipLength, _1: tipWidth / 2});
			var rightPoint = A2(
				_opensolid$geometry$OpenSolid_Point2d$in_,
				frame,
				{ctor: '_Tuple2', _0: _p22 - tipLength, _1: (0 - tipWidth) / 2});
			var tip = _opensolid$geometry$OpenSolid_Geometry_Types$Triangle2d(
				{ctor: '_Tuple3', _0: rightPoint, _1: tipPoint, _2: leftPoint});
			return A2(
				_elm_lang$svg$Svg$g,
				options.groupAttributes,
				{
					ctor: '::',
					_0: A2(_opensolid$svg$OpenSolid_Svg$lineSegment2d, options.stemAttributes, stem),
					_1: {
						ctor: '::',
						_0: A2(_opensolid$svg$OpenSolid_Svg$triangle2d, options.tipAttributes, tip),
						_1: {ctor: '[]'}
					}
				});
		} else {
			return _elm_lang$svg$Svg$text('');
		}
	});
var _opensolid$svg$OpenSolid_Svg$direction2d = F3(
	function (options, basePoint, direction) {
		return A3(
			_opensolid$svg$OpenSolid_Svg$vector2d,
			{tipLength: options.tipLength, tipWidth: options.tipWidth, stemAttributes: options.stemAttributes, tipAttributes: options.tipAttributes, groupAttributes: options.groupAttributes},
			basePoint,
			A2(_opensolid$geometry$OpenSolid_Vector2d$in_, direction, options.length));
	});
var _opensolid$svg$OpenSolid_Svg$polyline2d = F2(
	function (attributes, polyline) {
		var vertices = _opensolid$geometry$OpenSolid_Polyline2d$vertices(polyline);
		return A2(
			_elm_lang$svg$Svg$polyline,
			{
				ctor: '::',
				_0: _opensolid$svg$OpenSolid_Svg$pointsAttribute(vertices),
				_1: attributes
			},
			{ctor: '[]'});
	});
var _opensolid$svg$OpenSolid_Svg$polygon2d = F2(
	function (attributes, polygon) {
		var vertices = _opensolid$geometry$OpenSolid_Polygon2d$vertices(polygon);
		return A2(
			_elm_lang$svg$Svg$polygon,
			{
				ctor: '::',
				_0: _opensolid$svg$OpenSolid_Svg$pointsAttribute(vertices),
				_1: attributes
			},
			{ctor: '[]'});
	});
var _opensolid$svg$OpenSolid_Svg$render2d = F2(
	function (boundingBox, svg) {
		var _p23 = _opensolid$geometry$OpenSolid_BoundingBox2d$dimensions(boundingBox);
		var width = _p23._0;
		var height = _p23._1;
		var _p24 = _opensolid$geometry$OpenSolid_BoundingBox2d$extrema(boundingBox);
		var minX = _p24.minX;
		var maxY = _p24.maxY;
		var topLeftFrame = _opensolid$geometry$OpenSolid_Geometry_Types$Frame2d(
			{
				originPoint: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
					{ctor: '_Tuple2', _0: minX, _1: maxY}),
				xDirection: _opensolid$geometry$OpenSolid_Direction2d$positiveX,
				yDirection: _opensolid$geometry$OpenSolid_Direction2d$negativeY
			});
		return A2(
			_elm_lang$svg$Svg$svg,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$width(
					_elm_lang$core$Basics$toString(width)),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$height(
						_elm_lang$core$Basics$toString(height)),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: A2(_opensolid$svg$OpenSolid_Svg$relativeTo, topLeftFrame, svg),
				_1: {ctor: '[]'}
			});
	});
var _opensolid$svg$OpenSolid_Svg$VectorOptions = F5(
	function (a, b, c, d, e) {
		return {tipWidth: a, tipLength: b, stemAttributes: c, tipAttributes: d, groupAttributes: e};
	});
var _opensolid$svg$OpenSolid_Svg$DirectionOptions = F6(
	function (a, b, c, d, e, f) {
		return {length: a, tipWidth: b, tipLength: c, stemAttributes: d, tipAttributes: e, groupAttributes: f};
	});
var _opensolid$svg$OpenSolid_Svg$PointOptions = F2(
	function (a, b) {
		return {radius: a, attributes: b};
	});

var _zaboco$elm_draggable$Cmd_Extra$message = function (x) {
	return A2(
		_elm_lang$core$Task$perform,
		_elm_lang$core$Basics$identity,
		_elm_lang$core$Task$succeed(x));
};
var _zaboco$elm_draggable$Cmd_Extra$multiMessage = function (xs) {
	return _elm_lang$core$Platform_Cmd$batch(
		A2(_elm_lang$core$List$map, _zaboco$elm_draggable$Cmd_Extra$message, xs));
};
var _zaboco$elm_draggable$Cmd_Extra$optionalMessage = function (msgMaybe) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		_elm_lang$core$Platform_Cmd$none,
		A2(_elm_lang$core$Maybe$map, _zaboco$elm_draggable$Cmd_Extra$message, msgMaybe));
};

var _zaboco$elm_draggable$Internal$logInvalidState = F3(
	function (drag, msg, result) {
		var str = A2(
			_elm_lang$core$String$join,
			'',
			{
				ctor: '::',
				_0: 'Invalid drag state: ',
				_1: {
					ctor: '::',
					_0: _elm_lang$core$Basics$toString(drag),
					_1: {
						ctor: '::',
						_0: ': ',
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Basics$toString(msg),
							_1: {ctor: '[]'}
						}
					}
				}
			});
		var _p0 = _elm_lang$core$Debug$log(str);
		return result;
	});
var _zaboco$elm_draggable$Internal$distanceTo = F2(
	function (end, start) {
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Basics$toFloat(end.x - start.x),
			_1: _elm_lang$core$Basics$toFloat(end.y - start.y)
		};
	});
var _zaboco$elm_draggable$Internal$defaultConfig = {
	onDragStart: function (_p1) {
		return _elm_lang$core$Maybe$Nothing;
	},
	onDragBy: function (_p2) {
		return _elm_lang$core$Maybe$Nothing;
	},
	onDragEnd: _elm_lang$core$Maybe$Nothing,
	onClick: function (_p3) {
		return _elm_lang$core$Maybe$Nothing;
	},
	onMouseDown: function (_p4) {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _zaboco$elm_draggable$Internal$Config = F5(
	function (a, b, c, d, e) {
		return {onDragStart: a, onDragBy: b, onDragEnd: c, onClick: d, onMouseDown: e};
	});
var _zaboco$elm_draggable$Internal$Dragging = function (a) {
	return {ctor: 'Dragging', _0: a};
};
var _zaboco$elm_draggable$Internal$DraggingTentative = F2(
	function (a, b) {
		return {ctor: 'DraggingTentative', _0: a, _1: b};
	});
var _zaboco$elm_draggable$Internal$NotDragging = {ctor: 'NotDragging'};
var _zaboco$elm_draggable$Internal$updateAndEmit = F3(
	function (config, msg, drag) {
		var _p5 = {ctor: '_Tuple2', _0: drag, _1: msg};
		_v0_5:
		do {
			if (_p5.ctor === '_Tuple2') {
				switch (_p5._0.ctor) {
					case 'NotDragging':
						if (_p5._1.ctor === 'StartDragging') {
							var _p6 = _p5._1._0;
							return {
								ctor: '_Tuple2',
								_0: A2(_zaboco$elm_draggable$Internal$DraggingTentative, _p6, _p5._1._1),
								_1: config.onMouseDown(_p6)
							};
						} else {
							break _v0_5;
						}
					case 'DraggingTentative':
						switch (_p5._1.ctor) {
							case 'DragAt':
								return {
									ctor: '_Tuple2',
									_0: _zaboco$elm_draggable$Internal$Dragging(_p5._0._1),
									_1: config.onDragStart(_p5._0._0)
								};
							case 'StopDragging':
								return {
									ctor: '_Tuple2',
									_0: _zaboco$elm_draggable$Internal$NotDragging,
									_1: config.onClick(_p5._0._0)
								};
							default:
								break _v0_5;
						}
					default:
						switch (_p5._1.ctor) {
							case 'DragAt':
								var _p7 = _p5._1._0;
								return {
									ctor: '_Tuple2',
									_0: _zaboco$elm_draggable$Internal$Dragging(_p7),
									_1: config.onDragBy(
										A2(_zaboco$elm_draggable$Internal$distanceTo, _p7, _p5._0._0))
								};
							case 'StopDragging':
								return {ctor: '_Tuple2', _0: _zaboco$elm_draggable$Internal$NotDragging, _1: config.onDragEnd};
							default:
								break _v0_5;
						}
				}
			} else {
				break _v0_5;
			}
		} while(false);
		return A3(
			_zaboco$elm_draggable$Internal$logInvalidState,
			drag,
			msg,
			{ctor: '_Tuple2', _0: drag, _1: _elm_lang$core$Maybe$Nothing});
	});
var _zaboco$elm_draggable$Internal$StopDragging = {ctor: 'StopDragging'};
var _zaboco$elm_draggable$Internal$DragAt = function (a) {
	return {ctor: 'DragAt', _0: a};
};
var _zaboco$elm_draggable$Internal$StartDragging = F2(
	function (a, b) {
		return {ctor: 'StartDragging', _0: a, _1: b};
	});

var _zaboco$elm_draggable$Draggable$whenLeftMouseButtonPressed = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		function (button) {
			var _p0 = button;
			if (_p0 === 0) {
				return decoder;
			} else {
				return _elm_lang$core$Json_Decode$fail('Event is only relevant when the main mouse button was pressed.');
			}
		},
		A2(_elm_lang$core$Json_Decode$field, 'button', _elm_lang$core$Json_Decode$int));
};
var _zaboco$elm_draggable$Draggable$ignoreDefaults = A2(_elm_lang$virtual_dom$VirtualDom$Options, true, true);
var _zaboco$elm_draggable$Draggable$State = function (a) {
	return {ctor: 'State', _0: a};
};
var _zaboco$elm_draggable$Draggable$init = _zaboco$elm_draggable$Draggable$State(_zaboco$elm_draggable$Internal$NotDragging);
var _zaboco$elm_draggable$Draggable$updateDraggable = F3(
	function (_p3, _p2, _p1) {
		var _p4 = _p3;
		var _p5 = _p2;
		var _p6 = _p1;
		var _p7 = A3(_zaboco$elm_draggable$Internal$updateAndEmit, _p4._0, _p5._0, _p6._0);
		var newDrag = _p7._0;
		var newMsgMaybe = _p7._1;
		return {
			ctor: '_Tuple2',
			_0: _zaboco$elm_draggable$Draggable$State(newDrag),
			_1: _zaboco$elm_draggable$Cmd_Extra$optionalMessage(newMsgMaybe)
		};
	});
var _zaboco$elm_draggable$Draggable$update = F3(
	function (config, msg, model) {
		var _p8 = A3(_zaboco$elm_draggable$Draggable$updateDraggable, config, msg, model.drag);
		var dragState = _p8._0;
		var dragCmd = _p8._1;
		return A2(
			_elm_lang$core$Platform_Cmd_ops['!'],
			_elm_lang$core$Native_Utils.update(
				model,
				{drag: dragState}),
			{
				ctor: '::',
				_0: dragCmd,
				_1: {ctor: '[]'}
			});
	});
var _zaboco$elm_draggable$Draggable$Msg = function (a) {
	return {ctor: 'Msg', _0: a};
};
var _zaboco$elm_draggable$Draggable$subscriptions = F2(
	function (envelope, _p9) {
		var _p10 = _p9;
		var _p11 = _p10._0;
		if (_p11.ctor === 'NotDragging') {
			return _elm_lang$core$Platform_Sub$none;
		} else {
			return A2(
				_elm_lang$core$Platform_Sub$map,
				function (_p12) {
					return envelope(
						_zaboco$elm_draggable$Draggable$Msg(_p12));
				},
				_elm_lang$core$Platform_Sub$batch(
					{
						ctor: '::',
						_0: _elm_lang$mouse$Mouse$moves(_zaboco$elm_draggable$Internal$DragAt),
						_1: {
							ctor: '::',
							_0: _elm_lang$mouse$Mouse$ups(
								function (_p13) {
									return _zaboco$elm_draggable$Internal$StopDragging;
								}),
							_1: {ctor: '[]'}
						}
					}));
		}
	});
var _zaboco$elm_draggable$Draggable$positionDecoder = function (key) {
	return _zaboco$elm_draggable$Draggable$whenLeftMouseButtonPressed(
		A2(
			_elm_lang$core$Json_Decode$map,
			function (_p14) {
				return _zaboco$elm_draggable$Draggable$Msg(
					A2(_zaboco$elm_draggable$Internal$StartDragging, key, _p14));
			},
			_elm_lang$mouse$Mouse$position));
};
var _zaboco$elm_draggable$Draggable$mouseTrigger = F2(
	function (key, envelope) {
		return A3(
			_elm_lang$virtual_dom$VirtualDom$onWithOptions,
			'mousedown',
			_zaboco$elm_draggable$Draggable$ignoreDefaults,
			A2(
				_elm_lang$core$Json_Decode$map,
				envelope,
				_zaboco$elm_draggable$Draggable$positionDecoder(key)));
	});
var _zaboco$elm_draggable$Draggable$customMouseTrigger = F2(
	function (customDecoder, customEnvelope) {
		return A3(
			_elm_lang$virtual_dom$VirtualDom$onWithOptions,
			'mousedown',
			_zaboco$elm_draggable$Draggable$ignoreDefaults,
			A3(
				_elm_lang$core$Json_Decode$map2,
				customEnvelope,
				_zaboco$elm_draggable$Draggable$positionDecoder(
					{ctor: '_Tuple0'}),
				customDecoder));
	});
var _zaboco$elm_draggable$Draggable$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _zaboco$elm_draggable$Draggable$basicConfig = function (onDragByListener) {
	var defaultConfig = _zaboco$elm_draggable$Internal$defaultConfig;
	return _zaboco$elm_draggable$Draggable$Config(
		_elm_lang$core$Native_Utils.update(
			defaultConfig,
			{
				onDragBy: function (_p15) {
					return _elm_lang$core$Maybe$Just(
						onDragByListener(_p15));
				}
			}));
};
var _zaboco$elm_draggable$Draggable$customConfig = function (events) {
	return _zaboco$elm_draggable$Draggable$Config(
		A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, y) {
					return x(y);
				}),
			_zaboco$elm_draggable$Internal$defaultConfig,
			events));
};

var _zaboco$elm_draggable$Draggable_Events$onMouseDown = F2(
	function (toMsg, config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{
				onMouseDown: function (_p0) {
					return _elm_lang$core$Maybe$Just(
						toMsg(_p0));
				}
			});
	});
var _zaboco$elm_draggable$Draggable_Events$onClick = F2(
	function (toMsg, config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{
				onClick: function (_p1) {
					return _elm_lang$core$Maybe$Just(
						toMsg(_p1));
				}
			});
	});
var _zaboco$elm_draggable$Draggable_Events$onDragBy = F2(
	function (toMsg, config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{
				onDragBy: function (_p2) {
					return _elm_lang$core$Maybe$Just(
						toMsg(_p2));
				}
			});
	});
var _zaboco$elm_draggable$Draggable_Events$onDragEnd = F2(
	function (toMsg, config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{
				onDragEnd: _elm_lang$core$Maybe$Just(toMsg)
			});
	});
var _zaboco$elm_draggable$Draggable_Events$onDragStart = F2(
	function (toMsg, config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{
				onDragStart: function (_p3) {
					return _elm_lang$core$Maybe$Just(
						toMsg(_p3));
				}
			});
	});

var _ohanhi$keyboard_extra$Keyboard_Extra$boolToInt = function (bool) {
	return bool ? 1 : 0;
};
var _ohanhi$keyboard_extra$Keyboard_Extra$remove = F2(
	function (code, list) {
		return A2(
			_elm_lang$core$List$filter,
			F2(
				function (x, y) {
					return !_elm_lang$core$Native_Utils.eq(x, y);
				})(code),
			list);
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$insert = F2(
	function (code, list) {
		return A2(
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			code,
			A2(_ohanhi$keyboard_extra$Keyboard_Extra$remove, code, list));
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$update = F2(
	function (msg, state) {
		var _p0 = msg;
		if (_p0.ctor === 'Down') {
			return A2(_ohanhi$keyboard_extra$Keyboard_Extra$insert, _p0._0, state);
		} else {
			return A2(_ohanhi$keyboard_extra$Keyboard_Extra$remove, _p0._0, state);
		}
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$Arrows = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$Up = function (a) {
	return {ctor: 'Up', _0: a};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$Down = function (a) {
	return {ctor: 'Down', _0: a};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$KeyUp = function (a) {
	return {ctor: 'KeyUp', _0: a};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$KeyDown = function (a) {
	return {ctor: 'KeyDown', _0: a};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$updateWithKeyChange = F2(
	function (msg, state) {
		var _p1 = msg;
		if (_p1.ctor === 'Down') {
			var _p2 = _p1._0;
			var nextState = A2(_ohanhi$keyboard_extra$Keyboard_Extra$insert, _p2, state);
			var change = (!_elm_lang$core$Native_Utils.eq(
				_elm_lang$core$List$length(nextState),
				_elm_lang$core$List$length(state))) ? _elm_lang$core$Maybe$Just(
				_ohanhi$keyboard_extra$Keyboard_Extra$KeyDown(_p2)) : _elm_lang$core$Maybe$Nothing;
			return {ctor: '_Tuple2', _0: nextState, _1: change};
		} else {
			var _p3 = _p1._0;
			var nextState = A2(_ohanhi$keyboard_extra$Keyboard_Extra$remove, _p3, state);
			var change = (!_elm_lang$core$Native_Utils.eq(
				_elm_lang$core$List$length(nextState),
				_elm_lang$core$List$length(state))) ? _elm_lang$core$Maybe$Just(
				_ohanhi$keyboard_extra$Keyboard_Extra$KeyUp(_p3)) : _elm_lang$core$Maybe$Nothing;
			return {ctor: '_Tuple2', _0: nextState, _1: change};
		}
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$NoDirection = {ctor: 'NoDirection'};
var _ohanhi$keyboard_extra$Keyboard_Extra$NorthWest = {ctor: 'NorthWest'};
var _ohanhi$keyboard_extra$Keyboard_Extra$West = {ctor: 'West'};
var _ohanhi$keyboard_extra$Keyboard_Extra$SouthWest = {ctor: 'SouthWest'};
var _ohanhi$keyboard_extra$Keyboard_Extra$South = {ctor: 'South'};
var _ohanhi$keyboard_extra$Keyboard_Extra$SouthEast = {ctor: 'SouthEast'};
var _ohanhi$keyboard_extra$Keyboard_Extra$East = {ctor: 'East'};
var _ohanhi$keyboard_extra$Keyboard_Extra$NorthEast = {ctor: 'NorthEast'};
var _ohanhi$keyboard_extra$Keyboard_Extra$North = {ctor: 'North'};
var _ohanhi$keyboard_extra$Keyboard_Extra$arrowsToDir = function (_p4) {
	var _p5 = _p4;
	var _p6 = {ctor: '_Tuple2', _0: _p5.x, _1: _p5.y};
	_v3_8:
	do {
		if (_p6.ctor === '_Tuple2') {
			switch (_p6._0) {
				case 1:
					switch (_p6._1) {
						case 1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$NorthEast;
						case 0:
							return _ohanhi$keyboard_extra$Keyboard_Extra$East;
						case -1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$SouthEast;
						default:
							break _v3_8;
					}
				case 0:
					switch (_p6._1) {
						case 1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$North;
						case -1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$South;
						default:
							break _v3_8;
					}
				case -1:
					switch (_p6._1) {
						case -1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$SouthWest;
						case 0:
							return _ohanhi$keyboard_extra$Keyboard_Extra$West;
						case 1:
							return _ohanhi$keyboard_extra$Keyboard_Extra$NorthWest;
						default:
							break _v3_8;
					}
				default:
					break _v3_8;
			}
		} else {
			break _v3_8;
		}
	} while(false);
	return _ohanhi$keyboard_extra$Keyboard_Extra$NoDirection;
};
var _ohanhi$keyboard_extra$Keyboard_Extra$Other = {ctor: 'Other'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Altgr = {ctor: 'Altgr'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Meta = {ctor: 'Meta'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Quote = {ctor: 'Quote'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CloseBracket = {ctor: 'CloseBracket'};
var _ohanhi$keyboard_extra$Keyboard_Extra$BackSlash = {ctor: 'BackSlash'};
var _ohanhi$keyboard_extra$Keyboard_Extra$OpenBracket = {ctor: 'OpenBracket'};
var _ohanhi$keyboard_extra$Keyboard_Extra$BackQuote = {ctor: 'BackQuote'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Slash = {ctor: 'Slash'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Period = {ctor: 'Period'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Minus = {ctor: 'Minus'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Comma = {ctor: 'Comma'};
var _ohanhi$keyboard_extra$Keyboard_Extra$VolumeUp = {ctor: 'VolumeUp'};
var _ohanhi$keyboard_extra$Keyboard_Extra$VolumeDown = {ctor: 'VolumeDown'};
var _ohanhi$keyboard_extra$Keyboard_Extra$VolumeMute = {ctor: 'VolumeMute'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Tilde = {ctor: 'Tilde'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CloseCurlyBracket = {ctor: 'CloseCurlyBracket'};
var _ohanhi$keyboard_extra$Keyboard_Extra$OpenCurlyBracket = {ctor: 'OpenCurlyBracket'};
var _ohanhi$keyboard_extra$Keyboard_Extra$HyphenMinus = {ctor: 'HyphenMinus'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Pipe = {ctor: 'Pipe'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Plus = {ctor: 'Plus'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Asterisk = {ctor: 'Asterisk'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CloseParen = {ctor: 'CloseParen'};
var _ohanhi$keyboard_extra$Keyboard_Extra$OpenParen = {ctor: 'OpenParen'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Underscore = {ctor: 'Underscore'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Ampersand = {ctor: 'Ampersand'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Percent = {ctor: 'Percent'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Dollar = {ctor: 'Dollar'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Hash = {ctor: 'Hash'};
var _ohanhi$keyboard_extra$Keyboard_Extra$DoubleQuote = {ctor: 'DoubleQuote'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Exclamation = {ctor: 'Exclamation'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Circumflex = {ctor: 'Circumflex'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ScrollLock = {ctor: 'ScrollLock'};
var _ohanhi$keyboard_extra$Keyboard_Extra$NumLock = {ctor: 'NumLock'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F24 = {ctor: 'F24'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F23 = {ctor: 'F23'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F22 = {ctor: 'F22'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F21 = {ctor: 'F21'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F20 = {ctor: 'F20'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F19 = {ctor: 'F19'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F18 = {ctor: 'F18'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F17 = {ctor: 'F17'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F16 = {ctor: 'F16'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F15 = {ctor: 'F15'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F14 = {ctor: 'F14'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F13 = {ctor: 'F13'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F12 = {ctor: 'F12'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F11 = {ctor: 'F11'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F10 = {ctor: 'F10'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F9 = {ctor: 'F9'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F8 = {ctor: 'F8'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F7 = {ctor: 'F7'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F6 = {ctor: 'F6'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F5 = {ctor: 'F5'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F4 = {ctor: 'F4'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F3 = {ctor: 'F3'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F2 = {ctor: 'F2'};
var _ohanhi$keyboard_extra$Keyboard_Extra$F1 = {ctor: 'F1'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Divide = {ctor: 'Divide'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Decimal = {ctor: 'Decimal'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Subtract = {ctor: 'Subtract'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Separator = {ctor: 'Separator'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Add = {ctor: 'Add'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Multiply = {ctor: 'Multiply'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad9 = {ctor: 'Numpad9'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad8 = {ctor: 'Numpad8'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad7 = {ctor: 'Numpad7'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad6 = {ctor: 'Numpad6'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad5 = {ctor: 'Numpad5'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad4 = {ctor: 'Numpad4'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad3 = {ctor: 'Numpad3'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad2 = {ctor: 'Numpad2'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad1 = {ctor: 'Numpad1'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Numpad0 = {ctor: 'Numpad0'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Sleep = {ctor: 'Sleep'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ContextMenu = {ctor: 'ContextMenu'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Super = {ctor: 'Super'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharZ = {ctor: 'CharZ'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharY = {ctor: 'CharY'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharX = {ctor: 'CharX'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharW = {ctor: 'CharW'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharV = {ctor: 'CharV'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharU = {ctor: 'CharU'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharT = {ctor: 'CharT'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharS = {ctor: 'CharS'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharR = {ctor: 'CharR'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharQ = {ctor: 'CharQ'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharP = {ctor: 'CharP'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharO = {ctor: 'CharO'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharN = {ctor: 'CharN'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharM = {ctor: 'CharM'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharL = {ctor: 'CharL'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharK = {ctor: 'CharK'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharJ = {ctor: 'CharJ'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharI = {ctor: 'CharI'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharH = {ctor: 'CharH'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharG = {ctor: 'CharG'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharF = {ctor: 'CharF'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharE = {ctor: 'CharE'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharD = {ctor: 'CharD'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharC = {ctor: 'CharC'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharB = {ctor: 'CharB'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CharA = {ctor: 'CharA'};
var _ohanhi$keyboard_extra$Keyboard_Extra$wasd = function (keys) {
	var toInt = function (key) {
		return _ohanhi$keyboard_extra$Keyboard_Extra$boolToInt(
			A2(_elm_lang$core$List$member, key, keys));
	};
	var x = toInt(_ohanhi$keyboard_extra$Keyboard_Extra$CharD) - toInt(_ohanhi$keyboard_extra$Keyboard_Extra$CharA);
	var y = toInt(_ohanhi$keyboard_extra$Keyboard_Extra$CharW) - toInt(_ohanhi$keyboard_extra$Keyboard_Extra$CharS);
	return {x: x, y: y};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$wasdDirection = function (_p7) {
	return _ohanhi$keyboard_extra$Keyboard_Extra$arrowsToDir(
		_ohanhi$keyboard_extra$Keyboard_Extra$wasd(_p7));
};
var _ohanhi$keyboard_extra$Keyboard_Extra$At = {ctor: 'At'};
var _ohanhi$keyboard_extra$Keyboard_Extra$QuestionMark = {ctor: 'QuestionMark'};
var _ohanhi$keyboard_extra$Keyboard_Extra$GreaterThan = {ctor: 'GreaterThan'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Equals = {ctor: 'Equals'};
var _ohanhi$keyboard_extra$Keyboard_Extra$LessThan = {ctor: 'LessThan'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Semicolon = {ctor: 'Semicolon'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Colon = {ctor: 'Colon'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number9 = {ctor: 'Number9'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number8 = {ctor: 'Number8'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number7 = {ctor: 'Number7'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number6 = {ctor: 'Number6'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number5 = {ctor: 'Number5'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number4 = {ctor: 'Number4'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number3 = {ctor: 'Number3'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number2 = {ctor: 'Number2'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number1 = {ctor: 'Number1'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Number0 = {ctor: 'Number0'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Delete = {ctor: 'Delete'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Insert = {ctor: 'Insert'};
var _ohanhi$keyboard_extra$Keyboard_Extra$PrintScreen = {ctor: 'PrintScreen'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Execute = {ctor: 'Execute'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Print = {ctor: 'Print'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Select = {ctor: 'Select'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ArrowDown = {ctor: 'ArrowDown'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ArrowRight = {ctor: 'ArrowRight'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ArrowUp = {ctor: 'ArrowUp'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ArrowLeft = {ctor: 'ArrowLeft'};
var _ohanhi$keyboard_extra$Keyboard_Extra$arrows = function (keys) {
	var toInt = function (key) {
		return _ohanhi$keyboard_extra$Keyboard_Extra$boolToInt(
			A2(_elm_lang$core$List$member, key, keys));
	};
	var x = toInt(_ohanhi$keyboard_extra$Keyboard_Extra$ArrowRight) - toInt(_ohanhi$keyboard_extra$Keyboard_Extra$ArrowLeft);
	var y = toInt(_ohanhi$keyboard_extra$Keyboard_Extra$ArrowUp) - toInt(_ohanhi$keyboard_extra$Keyboard_Extra$ArrowDown);
	return {x: x, y: y};
};
var _ohanhi$keyboard_extra$Keyboard_Extra$arrowsDirection = function (_p8) {
	return _ohanhi$keyboard_extra$Keyboard_Extra$arrowsToDir(
		_ohanhi$keyboard_extra$Keyboard_Extra$arrows(_p8));
};
var _ohanhi$keyboard_extra$Keyboard_Extra$Home = {ctor: 'Home'};
var _ohanhi$keyboard_extra$Keyboard_Extra$End = {ctor: 'End'};
var _ohanhi$keyboard_extra$Keyboard_Extra$PageDown = {ctor: 'PageDown'};
var _ohanhi$keyboard_extra$Keyboard_Extra$PageUp = {ctor: 'PageUp'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Space = {ctor: 'Space'};
var _ohanhi$keyboard_extra$Keyboard_Extra$ModeChange = {ctor: 'ModeChange'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Accept = {ctor: 'Accept'};
var _ohanhi$keyboard_extra$Keyboard_Extra$NonConvert = {ctor: 'NonConvert'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Convert = {ctor: 'Convert'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Escape = {ctor: 'Escape'};
var _ohanhi$keyboard_extra$Keyboard_Extra$CapsLock = {ctor: 'CapsLock'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Pause = {ctor: 'Pause'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Alt = {ctor: 'Alt'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Control = {ctor: 'Control'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Shift = {ctor: 'Shift'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Enter = {ctor: 'Enter'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Clear = {ctor: 'Clear'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Tab = {ctor: 'Tab'};
var _ohanhi$keyboard_extra$Keyboard_Extra$BackSpace = {ctor: 'BackSpace'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Help = {ctor: 'Help'};
var _ohanhi$keyboard_extra$Keyboard_Extra$Cancel = {ctor: 'Cancel'};
var _ohanhi$keyboard_extra$Keyboard_Extra$codeBook = {
	ctor: '::',
	_0: {ctor: '_Tuple2', _0: 3, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Cancel},
	_1: {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 6, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Help},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 8, _1: _ohanhi$keyboard_extra$Keyboard_Extra$BackSpace},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 9, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Tab},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 12, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Clear},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 13, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Enter},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 16, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Shift},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 17, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Control},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 18, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Alt},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 19, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Pause},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 20, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CapsLock},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 27, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Escape},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 28, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Convert},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 29, _1: _ohanhi$keyboard_extra$Keyboard_Extra$NonConvert},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 30, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Accept},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 31, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ModeChange},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 32, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Space},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 33, _1: _ohanhi$keyboard_extra$Keyboard_Extra$PageUp},
																		_1: {
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 34, _1: _ohanhi$keyboard_extra$Keyboard_Extra$PageDown},
																			_1: {
																				ctor: '::',
																				_0: {ctor: '_Tuple2', _0: 35, _1: _ohanhi$keyboard_extra$Keyboard_Extra$End},
																				_1: {
																					ctor: '::',
																					_0: {ctor: '_Tuple2', _0: 36, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Home},
																					_1: {
																						ctor: '::',
																						_0: {ctor: '_Tuple2', _0: 37, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ArrowLeft},
																						_1: {
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 38, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ArrowUp},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 39, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ArrowRight},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 40, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ArrowDown},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 41, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Select},
																										_1: {
																											ctor: '::',
																											_0: {ctor: '_Tuple2', _0: 42, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Print},
																											_1: {
																												ctor: '::',
																												_0: {ctor: '_Tuple2', _0: 43, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Execute},
																												_1: {
																													ctor: '::',
																													_0: {ctor: '_Tuple2', _0: 44, _1: _ohanhi$keyboard_extra$Keyboard_Extra$PrintScreen},
																													_1: {
																														ctor: '::',
																														_0: {ctor: '_Tuple2', _0: 45, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Insert},
																														_1: {
																															ctor: '::',
																															_0: {ctor: '_Tuple2', _0: 46, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Delete},
																															_1: {
																																ctor: '::',
																																_0: {ctor: '_Tuple2', _0: 48, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number0},
																																_1: {
																																	ctor: '::',
																																	_0: {ctor: '_Tuple2', _0: 49, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number1},
																																	_1: {
																																		ctor: '::',
																																		_0: {ctor: '_Tuple2', _0: 50, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number2},
																																		_1: {
																																			ctor: '::',
																																			_0: {ctor: '_Tuple2', _0: 51, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number3},
																																			_1: {
																																				ctor: '::',
																																				_0: {ctor: '_Tuple2', _0: 52, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number4},
																																				_1: {
																																					ctor: '::',
																																					_0: {ctor: '_Tuple2', _0: 53, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number5},
																																					_1: {
																																						ctor: '::',
																																						_0: {ctor: '_Tuple2', _0: 54, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number6},
																																						_1: {
																																							ctor: '::',
																																							_0: {ctor: '_Tuple2', _0: 55, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number7},
																																							_1: {
																																								ctor: '::',
																																								_0: {ctor: '_Tuple2', _0: 56, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number8},
																																								_1: {
																																									ctor: '::',
																																									_0: {ctor: '_Tuple2', _0: 57, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Number9},
																																									_1: {
																																										ctor: '::',
																																										_0: {ctor: '_Tuple2', _0: 58, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Colon},
																																										_1: {
																																											ctor: '::',
																																											_0: {ctor: '_Tuple2', _0: 59, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Semicolon},
																																											_1: {
																																												ctor: '::',
																																												_0: {ctor: '_Tuple2', _0: 60, _1: _ohanhi$keyboard_extra$Keyboard_Extra$LessThan},
																																												_1: {
																																													ctor: '::',
																																													_0: {ctor: '_Tuple2', _0: 61, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Equals},
																																													_1: {
																																														ctor: '::',
																																														_0: {ctor: '_Tuple2', _0: 62, _1: _ohanhi$keyboard_extra$Keyboard_Extra$GreaterThan},
																																														_1: {
																																															ctor: '::',
																																															_0: {ctor: '_Tuple2', _0: 63, _1: _ohanhi$keyboard_extra$Keyboard_Extra$QuestionMark},
																																															_1: {
																																																ctor: '::',
																																																_0: {ctor: '_Tuple2', _0: 64, _1: _ohanhi$keyboard_extra$Keyboard_Extra$At},
																																																_1: {
																																																	ctor: '::',
																																																	_0: {ctor: '_Tuple2', _0: 65, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharA},
																																																	_1: {
																																																		ctor: '::',
																																																		_0: {ctor: '_Tuple2', _0: 66, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharB},
																																																		_1: {
																																																			ctor: '::',
																																																			_0: {ctor: '_Tuple2', _0: 67, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharC},
																																																			_1: {
																																																				ctor: '::',
																																																				_0: {ctor: '_Tuple2', _0: 68, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharD},
																																																				_1: {
																																																					ctor: '::',
																																																					_0: {ctor: '_Tuple2', _0: 69, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharE},
																																																					_1: {
																																																						ctor: '::',
																																																						_0: {ctor: '_Tuple2', _0: 70, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharF},
																																																						_1: {
																																																							ctor: '::',
																																																							_0: {ctor: '_Tuple2', _0: 71, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharG},
																																																							_1: {
																																																								ctor: '::',
																																																								_0: {ctor: '_Tuple2', _0: 72, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharH},
																																																								_1: {
																																																									ctor: '::',
																																																									_0: {ctor: '_Tuple2', _0: 73, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharI},
																																																									_1: {
																																																										ctor: '::',
																																																										_0: {ctor: '_Tuple2', _0: 74, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharJ},
																																																										_1: {
																																																											ctor: '::',
																																																											_0: {ctor: '_Tuple2', _0: 75, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharK},
																																																											_1: {
																																																												ctor: '::',
																																																												_0: {ctor: '_Tuple2', _0: 76, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharL},
																																																												_1: {
																																																													ctor: '::',
																																																													_0: {ctor: '_Tuple2', _0: 77, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharM},
																																																													_1: {
																																																														ctor: '::',
																																																														_0: {ctor: '_Tuple2', _0: 78, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharN},
																																																														_1: {
																																																															ctor: '::',
																																																															_0: {ctor: '_Tuple2', _0: 79, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharO},
																																																															_1: {
																																																																ctor: '::',
																																																																_0: {ctor: '_Tuple2', _0: 80, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharP},
																																																																_1: {
																																																																	ctor: '::',
																																																																	_0: {ctor: '_Tuple2', _0: 81, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharQ},
																																																																	_1: {
																																																																		ctor: '::',
																																																																		_0: {ctor: '_Tuple2', _0: 82, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharR},
																																																																		_1: {
																																																																			ctor: '::',
																																																																			_0: {ctor: '_Tuple2', _0: 83, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharS},
																																																																			_1: {
																																																																				ctor: '::',
																																																																				_0: {ctor: '_Tuple2', _0: 84, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharT},
																																																																				_1: {
																																																																					ctor: '::',
																																																																					_0: {ctor: '_Tuple2', _0: 85, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharU},
																																																																					_1: {
																																																																						ctor: '::',
																																																																						_0: {ctor: '_Tuple2', _0: 86, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharV},
																																																																						_1: {
																																																																							ctor: '::',
																																																																							_0: {ctor: '_Tuple2', _0: 87, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharW},
																																																																							_1: {
																																																																								ctor: '::',
																																																																								_0: {ctor: '_Tuple2', _0: 88, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharX},
																																																																								_1: {
																																																																									ctor: '::',
																																																																									_0: {ctor: '_Tuple2', _0: 89, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharY},
																																																																									_1: {
																																																																										ctor: '::',
																																																																										_0: {ctor: '_Tuple2', _0: 90, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CharZ},
																																																																										_1: {
																																																																											ctor: '::',
																																																																											_0: {ctor: '_Tuple2', _0: 91, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Super},
																																																																											_1: {
																																																																												ctor: '::',
																																																																												_0: {ctor: '_Tuple2', _0: 93, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ContextMenu},
																																																																												_1: {
																																																																													ctor: '::',
																																																																													_0: {ctor: '_Tuple2', _0: 95, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Sleep},
																																																																													_1: {
																																																																														ctor: '::',
																																																																														_0: {ctor: '_Tuple2', _0: 96, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad0},
																																																																														_1: {
																																																																															ctor: '::',
																																																																															_0: {ctor: '_Tuple2', _0: 97, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad1},
																																																																															_1: {
																																																																																ctor: '::',
																																																																																_0: {ctor: '_Tuple2', _0: 98, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad2},
																																																																																_1: {
																																																																																	ctor: '::',
																																																																																	_0: {ctor: '_Tuple2', _0: 99, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad3},
																																																																																	_1: {
																																																																																		ctor: '::',
																																																																																		_0: {ctor: '_Tuple2', _0: 100, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad4},
																																																																																		_1: {
																																																																																			ctor: '::',
																																																																																			_0: {ctor: '_Tuple2', _0: 101, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad5},
																																																																																			_1: {
																																																																																				ctor: '::',
																																																																																				_0: {ctor: '_Tuple2', _0: 102, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad6},
																																																																																				_1: {
																																																																																					ctor: '::',
																																																																																					_0: {ctor: '_Tuple2', _0: 103, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad7},
																																																																																					_1: {
																																																																																						ctor: '::',
																																																																																						_0: {ctor: '_Tuple2', _0: 104, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad8},
																																																																																						_1: {
																																																																																							ctor: '::',
																																																																																							_0: {ctor: '_Tuple2', _0: 105, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Numpad9},
																																																																																							_1: {
																																																																																								ctor: '::',
																																																																																								_0: {ctor: '_Tuple2', _0: 106, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Multiply},
																																																																																								_1: {
																																																																																									ctor: '::',
																																																																																									_0: {ctor: '_Tuple2', _0: 107, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Add},
																																																																																									_1: {
																																																																																										ctor: '::',
																																																																																										_0: {ctor: '_Tuple2', _0: 108, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Separator},
																																																																																										_1: {
																																																																																											ctor: '::',
																																																																																											_0: {ctor: '_Tuple2', _0: 109, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Subtract},
																																																																																											_1: {
																																																																																												ctor: '::',
																																																																																												_0: {ctor: '_Tuple2', _0: 110, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Decimal},
																																																																																												_1: {
																																																																																													ctor: '::',
																																																																																													_0: {ctor: '_Tuple2', _0: 111, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Divide},
																																																																																													_1: {
																																																																																														ctor: '::',
																																																																																														_0: {ctor: '_Tuple2', _0: 112, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F1},
																																																																																														_1: {
																																																																																															ctor: '::',
																																																																																															_0: {ctor: '_Tuple2', _0: 113, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F2},
																																																																																															_1: {
																																																																																																ctor: '::',
																																																																																																_0: {ctor: '_Tuple2', _0: 114, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F3},
																																																																																																_1: {
																																																																																																	ctor: '::',
																																																																																																	_0: {ctor: '_Tuple2', _0: 115, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F4},
																																																																																																	_1: {
																																																																																																		ctor: '::',
																																																																																																		_0: {ctor: '_Tuple2', _0: 116, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F5},
																																																																																																		_1: {
																																																																																																			ctor: '::',
																																																																																																			_0: {ctor: '_Tuple2', _0: 117, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F6},
																																																																																																			_1: {
																																																																																																				ctor: '::',
																																																																																																				_0: {ctor: '_Tuple2', _0: 118, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F7},
																																																																																																				_1: {
																																																																																																					ctor: '::',
																																																																																																					_0: {ctor: '_Tuple2', _0: 119, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F8},
																																																																																																					_1: {
																																																																																																						ctor: '::',
																																																																																																						_0: {ctor: '_Tuple2', _0: 120, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F9},
																																																																																																						_1: {
																																																																																																							ctor: '::',
																																																																																																							_0: {ctor: '_Tuple2', _0: 121, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F10},
																																																																																																							_1: {
																																																																																																								ctor: '::',
																																																																																																								_0: {ctor: '_Tuple2', _0: 122, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F11},
																																																																																																								_1: {
																																																																																																									ctor: '::',
																																																																																																									_0: {ctor: '_Tuple2', _0: 123, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F12},
																																																																																																									_1: {
																																																																																																										ctor: '::',
																																																																																																										_0: {ctor: '_Tuple2', _0: 124, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F13},
																																																																																																										_1: {
																																																																																																											ctor: '::',
																																																																																																											_0: {ctor: '_Tuple2', _0: 125, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F14},
																																																																																																											_1: {
																																																																																																												ctor: '::',
																																																																																																												_0: {ctor: '_Tuple2', _0: 126, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F15},
																																																																																																												_1: {
																																																																																																													ctor: '::',
																																																																																																													_0: {ctor: '_Tuple2', _0: 127, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F16},
																																																																																																													_1: {
																																																																																																														ctor: '::',
																																																																																																														_0: {ctor: '_Tuple2', _0: 128, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F17},
																																																																																																														_1: {
																																																																																																															ctor: '::',
																																																																																																															_0: {ctor: '_Tuple2', _0: 129, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F18},
																																																																																																															_1: {
																																																																																																																ctor: '::',
																																																																																																																_0: {ctor: '_Tuple2', _0: 130, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F19},
																																																																																																																_1: {
																																																																																																																	ctor: '::',
																																																																																																																	_0: {ctor: '_Tuple2', _0: 131, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F20},
																																																																																																																	_1: {
																																																																																																																		ctor: '::',
																																																																																																																		_0: {ctor: '_Tuple2', _0: 132, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F21},
																																																																																																																		_1: {
																																																																																																																			ctor: '::',
																																																																																																																			_0: {ctor: '_Tuple2', _0: 133, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F22},
																																																																																																																			_1: {
																																																																																																																				ctor: '::',
																																																																																																																				_0: {ctor: '_Tuple2', _0: 134, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F23},
																																																																																																																				_1: {
																																																																																																																					ctor: '::',
																																																																																																																					_0: {ctor: '_Tuple2', _0: 135, _1: _ohanhi$keyboard_extra$Keyboard_Extra$F24},
																																																																																																																					_1: {
																																																																																																																						ctor: '::',
																																																																																																																						_0: {ctor: '_Tuple2', _0: 144, _1: _ohanhi$keyboard_extra$Keyboard_Extra$NumLock},
																																																																																																																						_1: {
																																																																																																																							ctor: '::',
																																																																																																																							_0: {ctor: '_Tuple2', _0: 145, _1: _ohanhi$keyboard_extra$Keyboard_Extra$ScrollLock},
																																																																																																																							_1: {
																																																																																																																								ctor: '::',
																																																																																																																								_0: {ctor: '_Tuple2', _0: 160, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Circumflex},
																																																																																																																								_1: {
																																																																																																																									ctor: '::',
																																																																																																																									_0: {ctor: '_Tuple2', _0: 161, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Exclamation},
																																																																																																																									_1: {
																																																																																																																										ctor: '::',
																																																																																																																										_0: {ctor: '_Tuple2', _0: 162, _1: _ohanhi$keyboard_extra$Keyboard_Extra$DoubleQuote},
																																																																																																																										_1: {
																																																																																																																											ctor: '::',
																																																																																																																											_0: {ctor: '_Tuple2', _0: 163, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Hash},
																																																																																																																											_1: {
																																																																																																																												ctor: '::',
																																																																																																																												_0: {ctor: '_Tuple2', _0: 164, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Dollar},
																																																																																																																												_1: {
																																																																																																																													ctor: '::',
																																																																																																																													_0: {ctor: '_Tuple2', _0: 165, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Percent},
																																																																																																																													_1: {
																																																																																																																														ctor: '::',
																																																																																																																														_0: {ctor: '_Tuple2', _0: 166, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Ampersand},
																																																																																																																														_1: {
																																																																																																																															ctor: '::',
																																																																																																																															_0: {ctor: '_Tuple2', _0: 167, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Underscore},
																																																																																																																															_1: {
																																																																																																																																ctor: '::',
																																																																																																																																_0: {ctor: '_Tuple2', _0: 168, _1: _ohanhi$keyboard_extra$Keyboard_Extra$OpenParen},
																																																																																																																																_1: {
																																																																																																																																	ctor: '::',
																																																																																																																																	_0: {ctor: '_Tuple2', _0: 169, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CloseParen},
																																																																																																																																	_1: {
																																																																																																																																		ctor: '::',
																																																																																																																																		_0: {ctor: '_Tuple2', _0: 170, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Asterisk},
																																																																																																																																		_1: {
																																																																																																																																			ctor: '::',
																																																																																																																																			_0: {ctor: '_Tuple2', _0: 171, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Plus},
																																																																																																																																			_1: {
																																																																																																																																				ctor: '::',
																																																																																																																																				_0: {ctor: '_Tuple2', _0: 172, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Pipe},
																																																																																																																																				_1: {
																																																																																																																																					ctor: '::',
																																																																																																																																					_0: {ctor: '_Tuple2', _0: 173, _1: _ohanhi$keyboard_extra$Keyboard_Extra$HyphenMinus},
																																																																																																																																					_1: {
																																																																																																																																						ctor: '::',
																																																																																																																																						_0: {ctor: '_Tuple2', _0: 174, _1: _ohanhi$keyboard_extra$Keyboard_Extra$OpenCurlyBracket},
																																																																																																																																						_1: {
																																																																																																																																							ctor: '::',
																																																																																																																																							_0: {ctor: '_Tuple2', _0: 175, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CloseCurlyBracket},
																																																																																																																																							_1: {
																																																																																																																																								ctor: '::',
																																																																																																																																								_0: {ctor: '_Tuple2', _0: 176, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Tilde},
																																																																																																																																								_1: {
																																																																																																																																									ctor: '::',
																																																																																																																																									_0: {ctor: '_Tuple2', _0: 181, _1: _ohanhi$keyboard_extra$Keyboard_Extra$VolumeMute},
																																																																																																																																									_1: {
																																																																																																																																										ctor: '::',
																																																																																																																																										_0: {ctor: '_Tuple2', _0: 182, _1: _ohanhi$keyboard_extra$Keyboard_Extra$VolumeDown},
																																																																																																																																										_1: {
																																																																																																																																											ctor: '::',
																																																																																																																																											_0: {ctor: '_Tuple2', _0: 183, _1: _ohanhi$keyboard_extra$Keyboard_Extra$VolumeUp},
																																																																																																																																											_1: {
																																																																																																																																												ctor: '::',
																																																																																																																																												_0: {ctor: '_Tuple2', _0: 186, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Semicolon},
																																																																																																																																												_1: {
																																																																																																																																													ctor: '::',
																																																																																																																																													_0: {ctor: '_Tuple2', _0: 187, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Equals},
																																																																																																																																													_1: {
																																																																																																																																														ctor: '::',
																																																																																																																																														_0: {ctor: '_Tuple2', _0: 188, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Comma},
																																																																																																																																														_1: {
																																																																																																																																															ctor: '::',
																																																																																																																																															_0: {ctor: '_Tuple2', _0: 189, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Minus},
																																																																																																																																															_1: {
																																																																																																																																																ctor: '::',
																																																																																																																																																_0: {ctor: '_Tuple2', _0: 190, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Period},
																																																																																																																																																_1: {
																																																																																																																																																	ctor: '::',
																																																																																																																																																	_0: {ctor: '_Tuple2', _0: 191, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Slash},
																																																																																																																																																	_1: {
																																																																																																																																																		ctor: '::',
																																																																																																																																																		_0: {ctor: '_Tuple2', _0: 192, _1: _ohanhi$keyboard_extra$Keyboard_Extra$BackQuote},
																																																																																																																																																		_1: {
																																																																																																																																																			ctor: '::',
																																																																																																																																																			_0: {ctor: '_Tuple2', _0: 219, _1: _ohanhi$keyboard_extra$Keyboard_Extra$OpenBracket},
																																																																																																																																																			_1: {
																																																																																																																																																				ctor: '::',
																																																																																																																																																				_0: {ctor: '_Tuple2', _0: 220, _1: _ohanhi$keyboard_extra$Keyboard_Extra$BackSlash},
																																																																																																																																																				_1: {
																																																																																																																																																					ctor: '::',
																																																																																																																																																					_0: {ctor: '_Tuple2', _0: 221, _1: _ohanhi$keyboard_extra$Keyboard_Extra$CloseBracket},
																																																																																																																																																					_1: {
																																																																																																																																																						ctor: '::',
																																																																																																																																																						_0: {ctor: '_Tuple2', _0: 222, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Quote},
																																																																																																																																																						_1: {
																																																																																																																																																							ctor: '::',
																																																																																																																																																							_0: {ctor: '_Tuple2', _0: 224, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Meta},
																																																																																																																																																							_1: {
																																																																																																																																																								ctor: '::',
																																																																																																																																																								_0: {ctor: '_Tuple2', _0: 225, _1: _ohanhi$keyboard_extra$Keyboard_Extra$Altgr},
																																																																																																																																																								_1: {ctor: '[]'}
																																																																																																																																																							}
																																																																																																																																																						}
																																																																																																																																																					}
																																																																																																																																																				}
																																																																																																																																																			}
																																																																																																																																																		}
																																																																																																																																																	}
																																																																																																																																																}
																																																																																																																																															}
																																																																																																																																														}
																																																																																																																																													}
																																																																																																																																												}
																																																																																																																																											}
																																																																																																																																										}
																																																																																																																																									}
																																																																																																																																								}
																																																																																																																																							}
																																																																																																																																						}
																																																																																																																																					}
																																																																																																																																				}
																																																																																																																																			}
																																																																																																																																		}
																																																																																																																																	}
																																																																																																																																}
																																																																																																																															}
																																																																																																																														}
																																																																																																																													}
																																																																																																																												}
																																																																																																																											}
																																																																																																																										}
																																																																																																																									}
																																																																																																																								}
																																																																																																																							}
																																																																																																																						}
																																																																																																																					}
																																																																																																																				}
																																																																																																																			}
																																																																																																																		}
																																																																																																																	}
																																																																																																																}
																																																																																																															}
																																																																																																														}
																																																																																																													}
																																																																																																												}
																																																																																																											}
																																																																																																										}
																																																																																																									}
																																																																																																								}
																																																																																																							}
																																																																																																						}
																																																																																																					}
																																																																																																				}
																																																																																																			}
																																																																																																		}
																																																																																																	}
																																																																																																}
																																																																																															}
																																																																																														}
																																																																																													}
																																																																																												}
																																																																																											}
																																																																																										}
																																																																																									}
																																																																																								}
																																																																																							}
																																																																																						}
																																																																																					}
																																																																																				}
																																																																																			}
																																																																																		}
																																																																																	}
																																																																																}
																																																																															}
																																																																														}
																																																																													}
																																																																												}
																																																																											}
																																																																										}
																																																																									}
																																																																								}
																																																																							}
																																																																						}
																																																																					}
																																																																				}
																																																																			}
																																																																		}
																																																																	}
																																																																}
																																																															}
																																																														}
																																																													}
																																																												}
																																																											}
																																																										}
																																																									}
																																																								}
																																																							}
																																																						}
																																																					}
																																																				}
																																																			}
																																																		}
																																																	}
																																																}
																																															}
																																														}
																																													}
																																												}
																																											}
																																										}
																																									}
																																								}
																																							}
																																						}
																																					}
																																				}
																																			}
																																		}
																																	}
																																}
																															}
																														}
																													}
																												}
																											}
																										}
																									}
																								}
																							}
																						}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
};
var _ohanhi$keyboard_extra$Keyboard_Extra$toCode = function (key) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		0,
		_elm_lang$core$List$head(
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(
					_elm_lang$core$List$filter,
					function (_p9) {
						return A2(
							F2(
								function (x, y) {
									return _elm_lang$core$Native_Utils.eq(x, y);
								}),
							key,
							_elm_lang$core$Tuple$second(_p9));
					},
					_ohanhi$keyboard_extra$Keyboard_Extra$codeBook))));
};
var _ohanhi$keyboard_extra$Keyboard_Extra$codeDict = _elm_lang$core$Dict$fromList(_ohanhi$keyboard_extra$Keyboard_Extra$codeBook);
var _ohanhi$keyboard_extra$Keyboard_Extra$fromCode = function (code) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		_ohanhi$keyboard_extra$Keyboard_Extra$Other,
		A2(_elm_lang$core$Dict$get, code, _ohanhi$keyboard_extra$Keyboard_Extra$codeDict));
};
var _ohanhi$keyboard_extra$Keyboard_Extra$downs = function (toMsg) {
	return _elm_lang$keyboard$Keyboard$downs(
		function (_p10) {
			return toMsg(
				_ohanhi$keyboard_extra$Keyboard_Extra$fromCode(_p10));
		});
};
var _ohanhi$keyboard_extra$Keyboard_Extra$ups = function (toMsg) {
	return _elm_lang$keyboard$Keyboard$ups(
		function (_p11) {
			return toMsg(
				_ohanhi$keyboard_extra$Keyboard_Extra$fromCode(_p11));
		});
};
var _ohanhi$keyboard_extra$Keyboard_Extra$subscriptions = _elm_lang$core$Platform_Sub$batch(
	{
		ctor: '::',
		_0: _elm_lang$keyboard$Keyboard$downs(
			function (_p12) {
				return _ohanhi$keyboard_extra$Keyboard_Extra$Down(
					_ohanhi$keyboard_extra$Keyboard_Extra$fromCode(_p12));
			}),
		_1: {
			ctor: '::',
			_0: _elm_lang$keyboard$Keyboard$ups(
				function (_p13) {
					return _ohanhi$keyboard_extra$Keyboard_Extra$Up(
						_ohanhi$keyboard_extra$Keyboard_Extra$fromCode(_p13));
				}),
			_1: {ctor: '[]'}
		}
	});
var _ohanhi$keyboard_extra$Keyboard_Extra$targetKey = A2(
	_elm_lang$core$Json_Decode$map,
	_ohanhi$keyboard_extra$Keyboard_Extra$fromCode,
	A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int));

var _jesseilev$graft$OpenSolid_Vector2d_Extra$updateComponents = function (updater) {
	return function (_p0) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
			updater(
				_opensolid$geometry$OpenSolid_Vector2d$components(_p0)));
	};
};
var _jesseilev$graft$OpenSolid_Vector2d_Extra$setX = function (newX) {
	return _jesseilev$graft$OpenSolid_Vector2d_Extra$updateComponents(
		_elm_lang$core$Tuple$mapFirst(
			function (_p1) {
				return newX;
			}));
};
var _jesseilev$graft$OpenSolid_Vector2d_Extra$setY = function (newY) {
	return _jesseilev$graft$OpenSolid_Vector2d_Extra$updateComponents(
		_elm_lang$core$Tuple$mapSecond(
			function (_p2) {
				return newY;
			}));
};

var _jesseilev$graft$OpenSolid_Extra$arcToPolygon = function (arc) {
	var pointAlongRim = function (_p0) {
		return A2(
			_opensolid$geometry$OpenSolid_Arc2d$point,
			arc,
			A3(
				_elm_lang$core$Basics$flip,
				F2(
					function (x, y) {
						return x / y;
					}),
				30,
				_elm_lang$core$Basics$toFloat(_p0)));
	};
	return _opensolid$geometry$OpenSolid_Geometry_Types$Polygon2d(
		A2(
			_elm_lang$core$Basics_ops['++'],
			{
				ctor: '::',
				_0: _opensolid$geometry$OpenSolid_Arc2d$endPoint(arc),
				_1: {
					ctor: '::',
					_0: _opensolid$geometry$OpenSolid_Arc2d$centerPoint(arc),
					_1: {
						ctor: '::',
						_0: _opensolid$geometry$OpenSolid_Arc2d$startPoint(arc),
						_1: {ctor: '[]'}
					}
				}
			},
			A2(
				_elm_lang$core$List$map,
				pointAlongRim,
				A2(_elm_lang$core$List$range, 0, 30))));
};
var _jesseilev$graft$OpenSolid_Extra$unitQuarterWedge = _opensolid$geometry$OpenSolid_Geometry_Types$Arc2d(
	{
		startPoint: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
			{ctor: '_Tuple2', _0: -1, _1: 0}),
		centerPoint: _opensolid$geometry$OpenSolid_Point2d$origin,
		sweptAngle: _elm_lang$core$Basics$degrees(90)
	});
var _jesseilev$graft$OpenSolid_Extra$unitHalfWedge = _opensolid$geometry$OpenSolid_Geometry_Types$Arc2d(
	{
		startPoint: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
			{ctor: '_Tuple2', _0: -1, _1: 0}),
		centerPoint: _opensolid$geometry$OpenSolid_Point2d$origin,
		sweptAngle: _elm_lang$core$Basics$degrees(180)
	});
var _jesseilev$graft$OpenSolid_Extra$unitTriangle = _opensolid$geometry$OpenSolid_Geometry_Types$Triangle2d(
	{
		ctor: '_Tuple3',
		_0: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
			{ctor: '_Tuple2', _0: -1, _1: -1}),
		_1: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
			{ctor: '_Tuple2', _0: -1, _1: 1}),
		_2: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
			{ctor: '_Tuple2', _0: 1, _1: -1})
	});
var _jesseilev$graft$OpenSolid_Extra$unitCircle = _opensolid$geometry$OpenSolid_Geometry_Types$Circle2d(
	{
		centerPoint: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
			{ctor: '_Tuple2', _0: 0, _1: 0}),
		radius: 1
	});
var _jesseilev$graft$OpenSolid_Extra$rectangle2d = F4(
	function (x, y, w, h) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Polygon2d(
			{
				ctor: '::',
				_0: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
					{ctor: '_Tuple2', _0: x, _1: y}),
				_1: {
					ctor: '::',
					_0: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
						{ctor: '_Tuple2', _0: x + w, _1: y}),
					_1: {
						ctor: '::',
						_0: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
							{ctor: '_Tuple2', _0: x + w, _1: y + h}),
						_1: {
							ctor: '::',
							_0: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
								{ctor: '_Tuple2', _0: x, _1: y + h}),
							_1: {ctor: '[]'}
						}
					}
				}
			});
	});
var _jesseilev$graft$OpenSolid_Extra$unitSquare = A4(_jesseilev$graft$OpenSolid_Extra$rectangle2d, -1, -1, 2, 2);
var _jesseilev$graft$OpenSolid_Extra$pointFromBoundingBox = F3(
	function (getX, getY, bb) {
		return _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
			{
				ctor: '_Tuple2',
				_0: getX(bb),
				_1: getY(bb)
			});
	});
var _jesseilev$graft$OpenSolid_Extra$boundingBoxOrOrigin = function (_p1) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		_opensolid$geometry$OpenSolid_BoundingBox2d$singleton(_opensolid$geometry$OpenSolid_Point2d$origin),
		_opensolid$geometry$OpenSolid_Polygon2d$boundingBox(_p1));
};
var _jesseilev$graft$OpenSolid_Extra$centroid = function (_p2) {
	return _opensolid$geometry$OpenSolid_BoundingBox2d$centroid(
		_jesseilev$graft$OpenSolid_Extra$boundingBoxOrOrigin(_p2));
};
var _jesseilev$graft$OpenSolid_Extra$pointFromPolygon = F2(
	function (getBoundingBoxX, getBoundingBoxY) {
		return function (_p3) {
			return A3(
				_jesseilev$graft$OpenSolid_Extra$pointFromBoundingBox,
				getBoundingBoxX,
				getBoundingBoxY,
				_jesseilev$graft$OpenSolid_Extra$boundingBoxOrOrigin(_p3));
		};
	});
var _jesseilev$graft$OpenSolid_Extra$topLeft = A2(_jesseilev$graft$OpenSolid_Extra$pointFromPolygon, _opensolid$geometry$OpenSolid_BoundingBox2d$minX, _opensolid$geometry$OpenSolid_BoundingBox2d$minY);
var _jesseilev$graft$OpenSolid_Extra$topRight = A2(_jesseilev$graft$OpenSolid_Extra$pointFromPolygon, _opensolid$geometry$OpenSolid_BoundingBox2d$maxX, _opensolid$geometry$OpenSolid_BoundingBox2d$minY);
var _jesseilev$graft$OpenSolid_Extra$edgeTop = function (poly) {
	return A3(
		_elm_lang$core$Basics$curry,
		_opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d,
		_jesseilev$graft$OpenSolid_Extra$topLeft(poly),
		_jesseilev$graft$OpenSolid_Extra$topRight(poly));
};
var _jesseilev$graft$OpenSolid_Extra$midTop = function (_p4) {
	return _opensolid$geometry$OpenSolid_LineSegment2d$midpoint(
		_jesseilev$graft$OpenSolid_Extra$edgeTop(_p4));
};
var _jesseilev$graft$OpenSolid_Extra$bottomLeft = A2(_jesseilev$graft$OpenSolid_Extra$pointFromPolygon, _opensolid$geometry$OpenSolid_BoundingBox2d$minX, _opensolid$geometry$OpenSolid_BoundingBox2d$maxY);
var _jesseilev$graft$OpenSolid_Extra$edgeLeft = function (poly) {
	return A3(
		_elm_lang$core$Basics$curry,
		_opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d,
		_jesseilev$graft$OpenSolid_Extra$topLeft(poly),
		_jesseilev$graft$OpenSolid_Extra$bottomLeft(poly));
};
var _jesseilev$graft$OpenSolid_Extra$midLeft = function (_p5) {
	return _opensolid$geometry$OpenSolid_LineSegment2d$midpoint(
		_jesseilev$graft$OpenSolid_Extra$edgeLeft(_p5));
};
var _jesseilev$graft$OpenSolid_Extra$bottomRight = A2(_jesseilev$graft$OpenSolid_Extra$pointFromPolygon, _opensolid$geometry$OpenSolid_BoundingBox2d$maxX, _opensolid$geometry$OpenSolid_BoundingBox2d$maxY);
var _jesseilev$graft$OpenSolid_Extra$edgeRight = function (poly) {
	return A3(
		_elm_lang$core$Basics$curry,
		_opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d,
		_jesseilev$graft$OpenSolid_Extra$topRight(poly),
		_jesseilev$graft$OpenSolid_Extra$bottomRight(poly));
};
var _jesseilev$graft$OpenSolid_Extra$midRight = function (_p6) {
	return _opensolid$geometry$OpenSolid_LineSegment2d$midpoint(
		_jesseilev$graft$OpenSolid_Extra$edgeRight(_p6));
};
var _jesseilev$graft$OpenSolid_Extra$edgeBottom = function (poly) {
	return A3(
		_elm_lang$core$Basics$curry,
		_opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d,
		_jesseilev$graft$OpenSolid_Extra$bottomLeft(poly),
		_jesseilev$graft$OpenSolid_Extra$bottomRight(poly));
};
var _jesseilev$graft$OpenSolid_Extra$midBottom = function (_p7) {
	return _opensolid$geometry$OpenSolid_LineSegment2d$midpoint(
		_jesseilev$graft$OpenSolid_Extra$edgeBottom(_p7));
};

var _jesseilev$graft$Types$getHoverableNodeId = function (hoverable) {
	var _p0 = hoverable;
	switch (_p0.ctor) {
		case 'NodeBox':
			return _elm_lang$core$Maybe$Just(_p0._0);
		case 'NodeShape':
			return _elm_lang$core$Maybe$Just(_p0._0);
		case 'IncomingPort':
			return _elm_lang$core$Maybe$Just(_p0._0);
		case 'OutgoingPort':
			return _elm_lang$core$Maybe$Just(_p0._0);
		default:
			return _elm_lang$core$Maybe$Nothing;
	}
};
var _jesseilev$graft$Types$modelLensGraph = A2(
	_arturopala$elm_monocle$Monocle_Lens$Lens,
	function (_) {
		return _.graph;
	},
	F2(
		function (g, m) {
			return _elm_lang$core$Native_Utils.update(
				m,
				{graph: g});
		}));
var _jesseilev$graft$Types$transformationLensTranslation = A2(
	_arturopala$elm_monocle$Monocle_Lens$Lens,
	function (_) {
		return _.translation;
	},
	F2(
		function (tl, tf) {
			return _elm_lang$core$Native_Utils.update(
				tf,
				{translation: tl});
		}));
var _jesseilev$graft$Types$transformationLensRotation = A2(
	_arturopala$elm_monocle$Monocle_Lens$Lens,
	function (_) {
		return _.rotation;
	},
	F2(
		function (r, t) {
			return _elm_lang$core$Native_Utils.update(
				t,
				{rotation: r});
		}));
var _jesseilev$graft$Types$transformationLensScale = A2(
	_arturopala$elm_monocle$Monocle_Lens$Lens,
	function (_) {
		return _.scale;
	},
	F2(
		function (s, t) {
			return _elm_lang$core$Native_Utils.update(
				t,
				{scale: s});
		}));
var _jesseilev$graft$Types$edgeLensTransformation = A2(
	_arturopala$elm_monocle$Monocle_Lens$Lens,
	function (_) {
		return _.label;
	},
	F2(
		function (t, e) {
			return _elm_lang$core$Native_Utils.update(
				e,
				{label: t});
		}));
var _jesseilev$graft$Types$edgeLensScale = A2(_arturopala$elm_monocle$Monocle_Lens$compose, _jesseilev$graft$Types$edgeLensTransformation, _jesseilev$graft$Types$transformationLensScale);
var _jesseilev$graft$Types$edgeLensRotation = A2(_arturopala$elm_monocle$Monocle_Lens$compose, _jesseilev$graft$Types$edgeLensTransformation, _jesseilev$graft$Types$transformationLensRotation);
var _jesseilev$graft$Types$edgeLensTranslation = A2(_arturopala$elm_monocle$Monocle_Lens$compose, _jesseilev$graft$Types$edgeLensTransformation, _jesseilev$graft$Types$transformationLensTranslation);
var _jesseilev$graft$Types$elementLensShape = A2(
	_arturopala$elm_monocle$Monocle_Lens$Lens,
	function (_) {
		return _.shape;
	},
	F2(
		function (s, e) {
			return _elm_lang$core$Native_Utils.update(
				e,
				{shape: s});
		}));
var _jesseilev$graft$Types$elementLensOpacity = A2(
	_arturopala$elm_monocle$Monocle_Lens$Lens,
	function (_) {
		return _.opacity;
	},
	F2(
		function (o, e) {
			return _elm_lang$core$Native_Utils.update(
				e,
				{opacity: o});
		}));
var _jesseilev$graft$Types$elementLensColor = A2(
	_arturopala$elm_monocle$Monocle_Lens$Lens,
	function (_) {
		return _.color;
	},
	F2(
		function (c, e) {
			return _elm_lang$core$Native_Utils.update(
				e,
				{color: c});
		}));
var _jesseilev$graft$Types$nodeLensElement = A2(
	_arturopala$elm_monocle$Monocle_Lens$Lens,
	function (_) {
		return _.label;
	},
	F2(
		function (l, n) {
			return _elm_lang$core$Native_Utils.update(
				n,
				{label: l});
		}));
var _jesseilev$graft$Types$nodeLensColor = A2(_arturopala$elm_monocle$Monocle_Lens$compose, _jesseilev$graft$Types$nodeLensElement, _jesseilev$graft$Types$elementLensColor);
var _jesseilev$graft$Types$nodeLensOpacity = A2(_arturopala$elm_monocle$Monocle_Lens$compose, _jesseilev$graft$Types$nodeLensElement, _jesseilev$graft$Types$elementLensOpacity);
var _jesseilev$graft$Types$nodeLensShape = A2(_arturopala$elm_monocle$Monocle_Lens$compose, _jesseilev$graft$Types$nodeLensElement, _jesseilev$graft$Types$elementLensShape);
var _jesseilev$graft$Types$Element = F4(
	function (a, b, c, d) {
		return {color: a, opacity: b, shape: c, controlLocation: d};
	});
var _jesseilev$graft$Types$Transformation = F3(
	function (a, b, c) {
		return {translation: a, scale: b, rotation: c};
	});
var _jesseilev$graft$Types$Model = F8(
	function (a, b, c, d, e, f, g, h) {
		return {graph: a, rootId: b, zoomScale: c, panOffset: d, drag: e, dragAction: f, hoverItem: g, selectedItem: h};
	});
var _jesseilev$graft$Types$QuarterWedge = {ctor: 'QuarterWedge'};
var _jesseilev$graft$Types$HalfWedge = {ctor: 'HalfWedge'};
var _jesseilev$graft$Types$Triangle = {ctor: 'Triangle'};
var _jesseilev$graft$Types$Square = {ctor: 'Square'};
var _jesseilev$graft$Types$Circle = {ctor: 'Circle'};
var _jesseilev$graft$Types$shapeFromString = function (s) {
	var _p1 = s;
	switch (_p1) {
		case 'Circle':
			return _elm_lang$core$Result$Ok(_jesseilev$graft$Types$Circle);
		case 'Triangle':
			return _elm_lang$core$Result$Ok(_jesseilev$graft$Types$Triangle);
		case 'Square':
			return _elm_lang$core$Result$Ok(_jesseilev$graft$Types$Square);
		case 'HalfWedge':
			return _elm_lang$core$Result$Ok(_jesseilev$graft$Types$HalfWedge);
		case 'QuarterWedge':
			return _elm_lang$core$Result$Ok(_jesseilev$graft$Types$QuarterWedge);
		default:
			return _elm_lang$core$Result$Err(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'\'',
					A2(_elm_lang$core$Basics_ops['++'], s, '\' is not a valid Shape type')));
	}
};
var _jesseilev$graft$Types$Pan = {ctor: 'Pan'};
var _jesseilev$graft$Types$EdgeChangeEndNode = F2(
	function (a, b) {
		return {ctor: 'EdgeChangeEndNode', _0: a, _1: b};
	});
var _jesseilev$graft$Types$MoveNodeControl = function (a) {
	return {ctor: 'MoveNodeControl', _0: a};
};
var _jesseilev$graft$Types$Edge = F2(
	function (a, b) {
		return {ctor: 'Edge', _0: a, _1: b};
	});
var _jesseilev$graft$Types$Node = function (a) {
	return {ctor: 'Node', _0: a};
};
var _jesseilev$graft$Types$IncomingPort = function (a) {
	return {ctor: 'IncomingPort', _0: a};
};
var _jesseilev$graft$Types$OutgoingPort = function (a) {
	return {ctor: 'OutgoingPort', _0: a};
};
var _jesseilev$graft$Types$EdgeArrow = F2(
	function (a, b) {
		return {ctor: 'EdgeArrow', _0: a, _1: b};
	});
var _jesseilev$graft$Types$EdgeLine = F2(
	function (a, b) {
		return {ctor: 'EdgeLine', _0: a, _1: b};
	});
var _jesseilev$graft$Types$NodeShape = function (a) {
	return {ctor: 'NodeShape', _0: a};
};
var _jesseilev$graft$Types$NodeBox = function (a) {
	return {ctor: 'NodeBox', _0: a};
};
var _jesseilev$graft$Types$NoOp = {ctor: 'NoOp'};
var _jesseilev$graft$Types$DragMsg = function (a) {
	return {ctor: 'DragMsg', _0: a};
};
var _jesseilev$graft$Types$OnDragBy = function (a) {
	return {ctor: 'OnDragBy', _0: a};
};
var _jesseilev$graft$Types$StopDragging = {ctor: 'StopDragging'};
var _jesseilev$graft$Types$StartDragging = function (a) {
	return {ctor: 'StartDragging', _0: a};
};
var _jesseilev$graft$Types$TranslationY = F2(
	function (a, b) {
		return {ctor: 'TranslationY', _0: a, _1: b};
	});
var _jesseilev$graft$Types$TranslationX = F2(
	function (a, b) {
		return {ctor: 'TranslationX', _0: a, _1: b};
	});
var _jesseilev$graft$Types$ChangeRotation = F2(
	function (a, b) {
		return {ctor: 'ChangeRotation', _0: a, _1: b};
	});
var _jesseilev$graft$Types$ChangeScale = F2(
	function (a, b) {
		return {ctor: 'ChangeScale', _0: a, _1: b};
	});
var _jesseilev$graft$Types$ChangeShape = F2(
	function (a, b) {
		return {ctor: 'ChangeShape', _0: a, _1: b};
	});
var _jesseilev$graft$Types$ChangeOpacity = F2(
	function (a, b) {
		return {ctor: 'ChangeOpacity', _0: a, _1: b};
	});
var _jesseilev$graft$Types$ChangeColor = F2(
	function (a, b) {
		return {ctor: 'ChangeColor', _0: a, _1: b};
	});
var _jesseilev$graft$Types$Delete = {ctor: 'Delete'};
var _jesseilev$graft$Types$Deselect = {ctor: 'Deselect'};
var _jesseilev$graft$Types$Select = function (a) {
	return {ctor: 'Select', _0: a};
};
var _jesseilev$graft$Types$StopHover = {ctor: 'StopHover'};
var _jesseilev$graft$Types$StartHover = function (a) {
	return {ctor: 'StartHover', _0: a};
};
var _jesseilev$graft$Types$ZoomOut = {ctor: 'ZoomOut'};
var _jesseilev$graft$Types$ZoomIn = {ctor: 'ZoomIn'};

var _myrho$elm_round$Round$funNum = F3(
	function (fun, s, fl) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			1 / 0,
			_elm_lang$core$Result$toMaybe(
				_elm_lang$core$String$toFloat(
					A2(fun, s, fl))));
	});
var _myrho$elm_round$Round$splitComma = function (str) {
	var _p0 = A2(_elm_lang$core$String$split, '.', str);
	if (_p0.ctor === '::') {
		if (_p0._1.ctor === '::') {
			return {ctor: '_Tuple2', _0: _p0._0, _1: _p0._1._0};
		} else {
			return {ctor: '_Tuple2', _0: _p0._0, _1: '0'};
		}
	} else {
		return {ctor: '_Tuple2', _0: '0', _1: '0'};
	}
};
var _myrho$elm_round$Round$toDecimal = function (fl) {
	var _p1 = A2(
		_elm_lang$core$String$split,
		'e',
		_elm_lang$core$Basics$toString(fl));
	if (_p1.ctor === '::') {
		if (_p1._1.ctor === '::') {
			var _p4 = _p1._1._0;
			var _p2 = function () {
				var hasSign = _elm_lang$core$Native_Utils.cmp(fl, 0) < 0;
				var _p3 = _myrho$elm_round$Round$splitComma(_p1._0);
				var b = _p3._0;
				var a = _p3._1;
				return {
					ctor: '_Tuple3',
					_0: hasSign ? '-' : '',
					_1: hasSign ? A2(_elm_lang$core$String$dropLeft, 1, b) : b,
					_2: a
				};
			}();
			var sign = _p2._0;
			var before = _p2._1;
			var after = _p2._2;
			var e = A2(
				_elm_lang$core$Maybe$withDefault,
				0,
				_elm_lang$core$Result$toMaybe(
					_elm_lang$core$String$toInt(
						A2(_elm_lang$core$String$startsWith, '+', _p4) ? A2(_elm_lang$core$String$dropLeft, 1, _p4) : _p4)));
			var newBefore = (_elm_lang$core$Native_Utils.cmp(e, 0) > -1) ? before : ((_elm_lang$core$Native_Utils.cmp(
				_elm_lang$core$Basics$abs(e),
				_elm_lang$core$String$length(before)) < 0) ? A2(
				_elm_lang$core$Basics_ops['++'],
				A2(
					_elm_lang$core$String$left,
					_elm_lang$core$String$length(before) - _elm_lang$core$Basics$abs(e),
					before),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'.',
					A2(
						_elm_lang$core$String$right,
						_elm_lang$core$Basics$abs(e),
						before))) : A2(
				_elm_lang$core$Basics_ops['++'],
				'0.',
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(
						_elm_lang$core$String$repeat,
						_elm_lang$core$Basics$abs(e) - _elm_lang$core$String$length(before),
						'0'),
					before)));
			var newAfter = (_elm_lang$core$Native_Utils.cmp(e, 0) < 1) ? after : ((_elm_lang$core$Native_Utils.cmp(
				e,
				_elm_lang$core$String$length(after)) < 0) ? A2(
				_elm_lang$core$Basics_ops['++'],
				A2(_elm_lang$core$String$left, e, after),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'.',
					A2(
						_elm_lang$core$String$right,
						_elm_lang$core$String$length(after) - e,
						after))) : A2(
				_elm_lang$core$Basics_ops['++'],
				after,
				A2(
					_elm_lang$core$String$repeat,
					e - _elm_lang$core$String$length(after),
					'0')));
			return A2(
				_elm_lang$core$Basics_ops['++'],
				sign,
				A2(_elm_lang$core$Basics_ops['++'], newBefore, newAfter));
		} else {
			return _p1._0;
		}
	} else {
		return '';
	}
};
var _myrho$elm_round$Round$truncate = function (n) {
	return (_elm_lang$core$Native_Utils.cmp(n, 0) < 0) ? _elm_lang$core$Basics$ceiling(n) : _elm_lang$core$Basics$floor(n);
};
var _myrho$elm_round$Round$roundFun = F3(
	function (functor, s, fl) {
		if (_elm_lang$core$Native_Utils.eq(s, 0)) {
			return _elm_lang$core$Basics$toString(
				functor(fl));
		} else {
			if (_elm_lang$core$Native_Utils.cmp(s, 0) < 0) {
				return function (r) {
					return (!_elm_lang$core$Native_Utils.eq(r, '0')) ? A2(
						_elm_lang$core$Basics_ops['++'],
						r,
						A2(
							_elm_lang$core$String$repeat,
							_elm_lang$core$Basics$abs(s),
							'0')) : r;
				}(
					A3(
						_myrho$elm_round$Round$roundFun,
						functor,
						0,
						A2(
							F2(
								function (x, y) {
									return x / y;
								}),
							fl,
							A2(
								F2(
									function (x, y) {
										return Math.pow(x, y);
									}),
								10,
								_elm_lang$core$Basics$abs(
									_elm_lang$core$Basics$toFloat(s))))));
			} else {
				var dd = (_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? 2 : 1;
				var n = (_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? -1 : 1;
				var e = Math.pow(10, s);
				var _p5 = _myrho$elm_round$Round$splitComma(
					_myrho$elm_round$Round$toDecimal(fl));
				var before = _p5._0;
				var after = _p5._1;
				var a = A3(
					_elm_lang$core$String$padRight,
					s + 1,
					_elm_lang$core$Native_Utils.chr('0'),
					after);
				var b = A2(_elm_lang$core$String$left, s, a);
				var c = A2(_elm_lang$core$String$dropLeft, s, a);
				var f = functor(
					A2(
						_elm_lang$core$Maybe$withDefault,
						_elm_lang$core$Basics$toFloat(e),
						_elm_lang$core$Result$toMaybe(
							_elm_lang$core$String$toFloat(
								A2(
									_elm_lang$core$Basics_ops['++'],
									(_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? '-' : '',
									A2(
										_elm_lang$core$Basics_ops['++'],
										'1',
										A2(
											_elm_lang$core$Basics_ops['++'],
											b,
											A2(_elm_lang$core$Basics_ops['++'], '.', c))))))));
				var g = A2(
					_elm_lang$core$String$dropLeft,
					dd,
					_elm_lang$core$Basics$toString(f));
				var h = _myrho$elm_round$Round$truncate(fl) + (_elm_lang$core$Native_Utils.eq(f - (e * n), e * n) ? ((_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? -1 : 1) : 0);
				var j = _elm_lang$core$Basics$toString(h);
				var i = (_elm_lang$core$Native_Utils.eq(j, '0') && ((!_elm_lang$core$Native_Utils.eq(f - (e * n), 0)) && ((_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) && (_elm_lang$core$Native_Utils.cmp(fl, -1) > 0)))) ? A2(_elm_lang$core$Basics_ops['++'], '-', j) : j;
				return A2(
					_elm_lang$core$Basics_ops['++'],
					i,
					A2(_elm_lang$core$Basics_ops['++'], '.', g));
			}
		}
	});
var _myrho$elm_round$Round$round = _myrho$elm_round$Round$roundFun(_elm_lang$core$Basics$round);
var _myrho$elm_round$Round$roundNum = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$round);
var _myrho$elm_round$Round$ceiling = _myrho$elm_round$Round$roundFun(_elm_lang$core$Basics$ceiling);
var _myrho$elm_round$Round$ceilingNum = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$ceiling);
var _myrho$elm_round$Round$floor = _myrho$elm_round$Round$roundFun(_elm_lang$core$Basics$floor);
var _myrho$elm_round$Round$floorCom = F2(
	function (s, fl) {
		return (_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? A2(_myrho$elm_round$Round$ceiling, s, fl) : A2(_myrho$elm_round$Round$floor, s, fl);
	});
var _myrho$elm_round$Round$floorNumCom = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$floorCom);
var _myrho$elm_round$Round$ceilingCom = F2(
	function (s, fl) {
		return (_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? A2(_myrho$elm_round$Round$floor, s, fl) : A2(_myrho$elm_round$Round$ceiling, s, fl);
	});
var _myrho$elm_round$Round$ceilingNumCom = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$ceilingCom);
var _myrho$elm_round$Round$floorNum = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$floor);
var _myrho$elm_round$Round$roundCom = _myrho$elm_round$Round$roundFun(
	function (fl) {
		var dec = fl - _elm_lang$core$Basics$toFloat(
			_myrho$elm_round$Round$truncate(fl));
		return (_elm_lang$core$Native_Utils.cmp(dec, 0.5) > -1) ? _elm_lang$core$Basics$ceiling(fl) : ((_elm_lang$core$Native_Utils.cmp(dec, -0.5) < 1) ? _elm_lang$core$Basics$floor(fl) : _elm_lang$core$Basics$round(fl));
	});
var _myrho$elm_round$Round$roundNumCom = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$roundCom);

var _jesseilev$graft$View$controlSize = 80;
var _jesseilev$graft$View$stageSize = 500;
var _jesseilev$graft$View$transformationDefault = {translation: _opensolid$geometry$OpenSolid_Vector2d$zero, scale: 0.5, rotation: 0};
var _jesseilev$graft$View$clickPositionDecoder = A3(
	_elm_lang$core$Json_Decode$map2,
	F2(
		function (x, y) {
			return _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
				{
					ctor: '_Tuple2',
					_0: _elm_lang$core$Basics$toFloat(x),
					_1: _elm_lang$core$Basics$toFloat(y)
				});
		}),
	A2(_elm_lang$core$Json_Decode$field, 'offsetX', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'offsetY', _elm_lang$core$Json_Decode$int));
var _jesseilev$graft$View$arrowTriangle = F2(
	function (tip, direction) {
		var back = A2(
			_opensolid$geometry$OpenSolid_Direction2d$scaleBy,
			_jesseilev$graft$View$controlSize / 4,
			_opensolid$geometry$OpenSolid_Direction2d$flip(direction));
		var left = A2(
			_opensolid$geometry$OpenSolid_Vector2d$scaleBy,
			0.5,
			_opensolid$geometry$OpenSolid_Vector2d$perpendicularTo(back));
		var right = A2(
			_opensolid$geometry$OpenSolid_Vector2d$rotateBy,
			_elm_lang$core$Basics$degrees(180),
			left);
		return _opensolid$geometry$OpenSolid_Geometry_Types$Triangle2d(
			{
				ctor: '_Tuple3',
				_0: tip,
				_1: A2(
					_opensolid$geometry$OpenSolid_Point2d$translateBy,
					A2(_opensolid$geometry$OpenSolid_Vector2d$sum, back, left),
					tip),
				_2: A2(
					_opensolid$geometry$OpenSolid_Point2d$translateBy,
					A2(_opensolid$geometry$OpenSolid_Vector2d$sum, back, right),
					tip)
			});
	});
var _jesseilev$graft$View$nodeControlRect = function (_p0) {
	var _p1 = _p0;
	return A4(
		_elm_lang$core$Basics$uncurry,
		_jesseilev$graft$OpenSolid_Extra$rectangle2d,
		_opensolid$geometry$OpenSolid_Point2d$coordinates(_p1.label.controlLocation),
		_jesseilev$graft$View$controlSize,
		_jesseilev$graft$View$controlSize);
};
var _jesseilev$graft$View$svgViewBoxString = F2(
	function (w, h) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			'0 0 ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(w),
				A2(
					_elm_lang$core$Basics_ops['++'],
					' ',
					_elm_lang$core$Basics$toString(h))));
	});
var _jesseilev$graft$View$acceptMaybe = F2(
	function ($default, func) {
		return function (_p2) {
			return A2(
				_elm_lang$core$Maybe$withDefault,
				$default,
				A2(_elm_lang$core$Maybe$map, func, _p2));
		};
	});
var _jesseilev$graft$View$floatMsgFromString = function (msgConstructor) {
	return function (_p3) {
		return A2(
			_elm_lang$core$Result$withDefault,
			_jesseilev$graft$Types$NoOp,
			A2(
				_elm_lang$core$Result$map,
				msgConstructor,
				_elm_lang$core$String$toFloat(_p3)));
	};
};
var _jesseilev$graft$View_ops = _jesseilev$graft$View_ops || {};
_jesseilev$graft$View_ops['=>'] = F2(
	function (v0, v1) {
		return {ctor: '_Tuple2', _0: v0, _1: v1};
	});
var _jesseilev$graft$View$outgoingPortLocation = function (_p4) {
	return A3(
		_elm_lang$core$Basics$flip,
		_opensolid$geometry$OpenSolid_LineSegment2d$interpolate,
		0.8,
		_jesseilev$graft$OpenSolid_Extra$edgeRight(
			_jesseilev$graft$View$nodeControlRect(_p4)));
};
var _jesseilev$graft$View$incomingPortLocation = function (_p5) {
	return A3(
		_elm_lang$core$Basics$flip,
		_opensolid$geometry$OpenSolid_LineSegment2d$interpolate,
		0.2,
		_jesseilev$graft$OpenSolid_Extra$edgeLeft(
			_jesseilev$graft$View$nodeControlRect(_p5)));
};
var _jesseilev$graft$View$unitShapeSvg = function (shape) {
	var _p6 = shape;
	switch (_p6.ctor) {
		case 'Circle':
			return A2(_elm_lang$core$Basics$flip, _opensolid$svg$OpenSolid_Svg$circle2d, _jesseilev$graft$OpenSolid_Extra$unitCircle);
		case 'Square':
			return A2(_elm_lang$core$Basics$flip, _opensolid$svg$OpenSolid_Svg$polygon2d, _jesseilev$graft$OpenSolid_Extra$unitSquare);
		case 'Triangle':
			return A2(_elm_lang$core$Basics$flip, _opensolid$svg$OpenSolid_Svg$triangle2d, _jesseilev$graft$OpenSolid_Extra$unitTriangle);
		case 'HalfWedge':
			return A2(
				_elm_lang$core$Basics$flip,
				_opensolid$svg$OpenSolid_Svg$polygon2d,
				_jesseilev$graft$OpenSolid_Extra$arcToPolygon(_jesseilev$graft$OpenSolid_Extra$unitHalfWedge));
		default:
			return A2(
				_elm_lang$core$Basics$flip,
				_opensolid$svg$OpenSolid_Svg$polygon2d,
				_jesseilev$graft$OpenSolid_Extra$arcToPolygon(_jesseilev$graft$OpenSolid_Extra$unitQuarterWedge));
	}
};
var _jesseilev$graft$View$fieldsetView = F2(
	function (labelText, child) {
		return A2(
			_elm_lang$html$Html$fieldset,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(
					{
						ctor: '::',
						_0: A2(_jesseilev$graft$View_ops['=>'], 'padding', '0.5 em'),
						_1: {
							ctor: '::',
							_0: A2(_jesseilev$graft$View_ops['=>'], 'padding-left', '0'),
							_1: {
								ctor: '::',
								_0: A2(_jesseilev$graft$View_ops['=>'], 'border-width', '0'),
								_1: {
									ctor: '::',
									_0: A2(_jesseilev$graft$View_ops['=>'], 'margin-top', '0.5em'),
									_1: {ctor: '[]'}
								}
							}
						}
					}),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$label,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(labelText),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: child,
					_1: {ctor: '[]'}
				}
			});
	});
var _jesseilev$graft$View$sliderView = F5(
	function (value, min, max, step, attrs) {
		return _elm_lang$html$Html$input(
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$type_('range'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$value(
							_elm_lang$core$Basics$toString(value)),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$min(
								_elm_lang$core$Basics$toString(min)),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$max(
									_elm_lang$core$Basics$toString(max)),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$step(
										_elm_lang$core$Basics$toString(step)),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$style(
											{
												ctor: '::',
												_0: A2(_jesseilev$graft$View_ops['=>'], '-webkit-appearance', 'none'),
												_1: {
													ctor: '::',
													_0: A2(_jesseilev$graft$View_ops['=>'], 'height', '2px'),
													_1: {
														ctor: '::',
														_0: A2(_jesseilev$graft$View_ops['=>'], 'background', '#aaa'),
														_1: {ctor: '[]'}
													}
												}
											}),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				},
				attrs));
	});
var _jesseilev$graft$View$shapeView = F2(
	function (node, attrs) {
		return A2(
			_jesseilev$graft$View$unitShapeSvg,
			node.label.shape,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$fill(
						_eskimoblood$elm_color_extra$Color_Convert$colorToHex(node.label.color)),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$opacity(
							_elm_lang$core$Basics$toString(node.label.opacity)),
						_1: {ctor: '[]'}
					}
				},
				attrs));
	});
var _jesseilev$graft$View$viewNodeDetail = function (_p7) {
	var _p8 = _p7;
	var _p10 = _p8;
	var _p9 = _p8.label;
	var shapeChooser = function (shape) {
		return A2(
			_elm_lang$svg$Svg$svg,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$viewBox('-1.25 -1.25 2.5 2.5'),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(
						{
							ctor: '::',
							_0: A2(_jesseilev$graft$View_ops['=>'], 'height', '50px'),
							_1: {
								ctor: '::',
								_0: A2(_jesseilev$graft$View_ops['=>'], 'margin-right', '8px'),
								_1: {ctor: '[]'}
							}
						}),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Events$onClick(
							A2(
								_jesseilev$graft$Types$ChangeShape,
								_p10,
								_elm_lang$core$Basics$toString(shape))),
						_1: {ctor: '[]'}
					}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_jesseilev$graft$View$shapeView,
					_elm_lang$core$Native_Utils.update(
						_p10,
						{
							label: _elm_lang$core$Native_Utils.update(
								_p9,
								{shape: shape, opacity: 1})
						}),
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$stroke(
							_elm_lang$core$Native_Utils.eq(_p9.shape, shape) ? _eskimoblood$elm_color_extra$Color_Convert$colorToHex(_p9.color) : '#aaa'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$strokeWidth('0.1'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$fill(
									_elm_lang$core$Native_Utils.eq(_p9.shape, shape) ? _eskimoblood$elm_color_extra$Color_Convert$colorToHex(_p9.color) : 'rgba(0,0,0,0)'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$cursor('pointer'),
									_1: {ctor: '[]'}
								}
							}
						}
					}),
				_1: {ctor: '[]'}
			});
	};
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_jesseilev$graft$View$fieldsetView,
				'',
				A2(
					_elm_lang$html$Html$div,
					{ctor: '[]'},
					A2(
						_elm_lang$core$List$map,
						shapeChooser,
						{
							ctor: '::',
							_0: _jesseilev$graft$Types$Square,
							_1: {
								ctor: '::',
								_0: _jesseilev$graft$Types$Triangle,
								_1: {
									ctor: '::',
									_0: _jesseilev$graft$Types$Circle,
									_1: {
										ctor: '::',
										_0: _jesseilev$graft$Types$HalfWedge,
										_1: {
											ctor: '::',
											_0: _jesseilev$graft$Types$QuarterWedge,
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}))),
			_1: {
				ctor: '::',
				_0: A2(
					_jesseilev$graft$View$fieldsetView,
					'',
					A2(
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$style(
								{
									ctor: '::',
									_0: A2(_jesseilev$graft$View_ops['=>'], 'display', 'grid'),
									_1: {
										ctor: '::',
										_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-template-columns', '25% 75%'),
										_1: {
											ctor: '::',
											_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-gap', '10px'),
											_1: {
												ctor: '::',
												_0: A2(_jesseilev$graft$View_ops['=>'], 'align-items', 'center'),
												_1: {ctor: '[]'}
											}
										}
									}
								}),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$input,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$type_('color'),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$value(
											_eskimoblood$elm_color_extra$Color_Convert$colorToHex(_p10.label.color)),
										_1: {
											ctor: '::',
											_0: _elm_lang$html$Html_Events$onInput(
												_jesseilev$graft$Types$ChangeColor(_p10)),
											_1: {
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$style(
													{
														ctor: '::',
														_0: A2(_jesseilev$graft$View_ops['=>'], 'width', '100%'),
														_1: {ctor: '[]'}
													}),
												_1: {ctor: '[]'}
											}
										}
									}
								},
								{ctor: '[]'}),
							_1: {
								ctor: '::',
								_0: A6(
									_jesseilev$graft$View$sliderView,
									_p10.label.opacity,
									0,
									1,
									1.0e-2,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Events$onInput(
											_jesseilev$graft$View$floatMsgFromString(
												_jesseilev$graft$Types$ChangeOpacity(_p10))),
										_1: {
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$style(
												{
													ctor: '::',
													_0: A2(
														_jesseilev$graft$View_ops['=>'],
														'background',
														A2(
															_elm_lang$core$Basics_ops['++'],
															'linear-gradient(to right, rgba(0,0,0,0), ',
															_eskimoblood$elm_color_extra$Color_Convert$colorToCssRgba(_p10.label.color))),
													_1: {ctor: '[]'}
												}),
											_1: {ctor: '[]'}
										}
									},
									{ctor: '[]'}),
								_1: {ctor: '[]'}
							}
						})),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$hr,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$style(
								{
									ctor: '::',
									_0: A2(_jesseilev$graft$View_ops['=>'], 'border-top', '1px solid #ccc'),
									_1: {
										ctor: '::',
										_0: A2(_jesseilev$graft$View_ops['=>'], 'border-bottom', '0'),
										_1: {
											ctor: '::',
											_0: A2(_jesseilev$graft$View_ops['=>'], 'margin-bottom', '20px'),
											_1: {ctor: '[]'}
										}
									}
								}),
							_1: {ctor: '[]'}
						},
						{ctor: '[]'}),
					_1: {
						ctor: '::',
						_0: A2(
							_jesseilev$graft$View$fieldsetView,
							'',
							A2(
								_elm_lang$html$Html$button,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Events$onClick(_jesseilev$graft$Types$Delete),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$style(
											{
												ctor: '::',
												_0: A2(_jesseilev$graft$View_ops['=>'], 'height', '40px'),
												_1: {
													ctor: '::',
													_0: A2(_jesseilev$graft$View_ops['=>'], 'width', '40px'),
													_1: {
														ctor: '::',
														_0: A2(_jesseilev$graft$View_ops['=>'], 'background', 'rgba(0,0,0,0)'),
														_1: {
															ctor: '::',
															_0: A2(_jesseilev$graft$View_ops['=>'], 'color', 'red'),
															_1: {
																ctor: '::',
																_0: A2(_jesseilev$graft$View_ops['=>'], 'font-family', 'Sans-serif'),
																_1: {
																	ctor: '::',
																	_0: A2(_jesseilev$graft$View_ops['=>'], 'font-size', '20px'),
																	_1: {
																		ctor: '::',
																		_0: A2(_jesseilev$graft$View_ops['=>'], 'float', 'right'),
																		_1: {
																			ctor: '::',
																			_0: A2(_jesseilev$graft$View_ops['=>'], 'border', '2px solid red'),
																			_1: {
																				ctor: '::',
																				_0: A2(_jesseilev$graft$View_ops['=>'], 'opacity', '0.5'),
																				_1: {ctor: '[]'}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}),
										_1: {ctor: '[]'}
									}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text('×'),
									_1: {ctor: '[]'}
								})),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _jesseilev$graft$View$viewEdgeDetail = F2(
	function (model, edge) {
		var translationY = _opensolid$geometry$OpenSolid_Vector2d$yComponent(
			function (_) {
				return _.translation;
			}(edge.label));
		var translationX = _opensolid$geometry$OpenSolid_Vector2d$xComponent(
			function (_) {
				return _.translation;
			}(edge.label));
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(
					{
						ctor: '::',
						_0: A2(_jesseilev$graft$View_ops['=>'], 'display', 'grid'),
						_1: {
							ctor: '::',
							_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-template-columns', '50% 50%'),
							_1: {
								ctor: '::',
								_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-gap', '10px'),
								_1: {ctor: '[]'}
							}
						}
					}),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A2(
							_jesseilev$graft$View$fieldsetView,
							A2(
								_elm_lang$core$Basics_ops['++'],
								'X: ',
								A2(_myrho$elm_round$Round$round, 2, translationX)),
							A6(
								_jesseilev$graft$View$sliderView,
								translationX,
								-2,
								2,
								5.0e-2,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Events$onInput(
										_jesseilev$graft$View$floatMsgFromString(
											_jesseilev$graft$Types$TranslationX(edge))),
									_1: {ctor: '[]'}
								},
								{ctor: '[]'})),
						_1: {
							ctor: '::',
							_0: A2(
								_jesseilev$graft$View$fieldsetView,
								A2(
									_elm_lang$core$Basics_ops['++'],
									'Y: ',
									A2(_myrho$elm_round$Round$round, 2, translationY)),
								A6(
									_jesseilev$graft$View$sliderView,
									translationY,
									-2,
									2,
									5.0e-2,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Events$onInput(
											_jesseilev$graft$View$floatMsgFromString(
												_jesseilev$graft$Types$TranslationY(edge))),
										_1: {ctor: '[]'}
									},
									{ctor: '[]'})),
							_1: {ctor: '[]'}
						}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$div,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: A2(
								_jesseilev$graft$View$fieldsetView,
								A2(
									_elm_lang$core$Basics_ops['++'],
									'Scale: ',
									A2(
										_myrho$elm_round$Round$round,
										2,
										function (_) {
											return _.scale;
										}(edge.label))),
								A6(
									_jesseilev$graft$View$sliderView,
									function (_) {
										return _.scale;
									}(edge.label),
									0,
									0.9,
									1.0e-2,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Events$onInput(
											_jesseilev$graft$View$floatMsgFromString(
												_jesseilev$graft$Types$ChangeScale(edge))),
										_1: {ctor: '[]'}
									},
									{ctor: '[]'})),
							_1: {
								ctor: '::',
								_0: A2(
									_jesseilev$graft$View$fieldsetView,
									A2(
										_elm_lang$core$Basics_ops['++'],
										'Rotation: ',
										_elm_lang$core$Basics$toString(
											function (_) {
												return _.rotation;
											}(edge.label))),
									A6(
										_jesseilev$graft$View$sliderView,
										function (_) {
											return _.rotation;
										}(edge.label),
										0,
										360,
										5,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Events$onInput(
												_jesseilev$graft$View$floatMsgFromString(
													_jesseilev$graft$Types$ChangeRotation(edge))),
											_1: {ctor: '[]'}
										},
										{ctor: '[]'})),
								_1: {ctor: '[]'}
							}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _jesseilev$graft$View$viewDetailsContainer = function (model) {
	return A2(
		_elm_lang$html$Html$section,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				{
					ctor: '::',
					_0: A2(_jesseilev$graft$View_ops['=>'], 'background', 'linear-gradient(to left, #ccc, #eee)'),
					_1: {
						ctor: '::',
						_0: A2(_jesseilev$graft$View_ops['=>'], 'padding', '30px'),
						_1: {ctor: '[]'}
					}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: function () {
				var _p11 = model.selectedItem;
				if (_p11.ctor === 'Just') {
					if (_p11._0.ctor === 'Node') {
						return A3(
							_jesseilev$graft$View$acceptMaybe,
							_elm_lang$html$Html$text(''),
							_jesseilev$graft$View$viewNodeDetail,
							A2(_jesseilev$graft$Graph_Extra$getNode, _p11._0._0, model.graph));
					} else {
						return A2(
							_jesseilev$graft$View$acceptMaybe,
							_elm_lang$html$Html$text(''),
							_jesseilev$graft$View$viewEdgeDetail(model))(
							A3(_jesseilev$graft$Graph_Extra$getEdge, _p11._0._0, _p11._0._1, model.graph));
					}
				} else {
					return _elm_lang$html$Html$text('');
				}
			}(),
			_1: {ctor: '[]'}
		});
};
var _jesseilev$graft$View$viewEdge = F2(
	function (model, edge) {
		var edgeCount = _elm_lang$core$List$length(
			_elm_community$graph$Graph$edges(model.graph));
		var chopALittleOffBothEnds = function (lineSeg) {
			return A3(
				_opensolid$geometry$OpenSolid_LineSegment2d$scaleAbout,
				_opensolid$geometry$OpenSolid_LineSegment2d$midpoint(lineSeg),
				0.95,
				lineSeg);
		};
		var specialDragEndpoint = function () {
			var _p12 = model.dragAction;
			if ((_p12.ctor === 'Just') && (_p12._0.ctor === 'EdgeChangeEndNode')) {
				return A2(_jesseilev$graft$Graph_Extra$edgeEquals, _p12._0._0, edge) ? _elm_lang$core$Maybe$Just(_p12._0._1) : _elm_lang$core$Maybe$Nothing;
			} else {
				return _elm_lang$core$Maybe$Nothing;
			}
		}();
		var getNode = A2(_elm_lang$core$Basics$flip, _elm_community$graph$Graph$get, model.graph);
		var arrowLocation = A2(_elm_lang$core$Basics$flip, _opensolid$geometry$OpenSolid_LineSegment2d$interpolate, 0.75);
		var arrowView = function (lineSeg) {
			return A2(
				_elm_lang$core$Maybe$withDefault,
				A2(
					_elm_lang$svg$Svg$g,
					{ctor: '[]'},
					{ctor: '[]'}),
				A2(
					_elm_lang$core$Maybe$map,
					_opensolid$svg$OpenSolid_Svg$triangle2d(
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$fill('grey'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$stroke('grey'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$cursor('-webkit-grab'),
									_1: {
										ctor: '::',
										_0: A2(
											_zaboco$elm_draggable$Draggable$mouseTrigger,
											A2(
												_jesseilev$graft$Types$EdgeChangeEndNode,
												edge,
												arrowLocation(lineSeg)),
											_jesseilev$graft$Types$DragMsg),
										_1: {ctor: '[]'}
									}
								}
							}
						}),
					A2(
						_elm_lang$core$Maybe$map,
						_jesseilev$graft$View$arrowTriangle(
							arrowLocation(lineSeg)),
						_opensolid$geometry$OpenSolid_LineSegment2d$direction(lineSeg))));
		};
		var isSelected = _elm_lang$core$Native_Utils.eq(
			model.selectedItem,
			_elm_lang$core$Maybe$Just(
				A2(_jesseilev$graft$Types$Edge, edge.from, edge.to)));
		var lineView = _opensolid$svg$OpenSolid_Svg$lineSegment2d(
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$stroke('grey'),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$strokeWidth(
						isSelected ? '2px' : '1px'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$strokeDasharray(
							isSelected ? '' : '5,5'),
						_1: {ctor: '[]'}
					}
				}
			});
		var edgeView = function (lineSeg) {
			return A2(
				_elm_lang$svg$Svg$g,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Events$onClick(
						_jesseilev$graft$Types$Select(
							A2(_jesseilev$graft$Types$Edge, edge.from, edge.to))),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A2(
						_opensolid$svg$OpenSolid_Svg$lineSegment2d,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$opacity(
								isSelected ? '0' : '0'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$strokeWidth('10px'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$stroke('yellow'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$cursor(
											isSelected ? 'default' : 'pointer'),
										_1: {ctor: '[]'}
									}
								}
							}
						},
						lineSeg),
					_1: {
						ctor: '::',
						_0: lineView(lineSeg),
						_1: {
							ctor: '::',
							_0: isSelected ? arrowView(lineSeg) : A2(
								_elm_lang$svg$Svg$g,
								{ctor: '[]'},
								{ctor: '[]'}),
							_1: {ctor: '[]'}
						}
					}
				});
		};
		var _p13 = {
			ctor: '_Tuple3',
			_0: getNode(edge.from),
			_1: getNode(edge.to),
			_2: specialDragEndpoint
		};
		_v5_2:
		do {
			if ((_p13.ctor === '_Tuple3') && (_p13._0.ctor === 'Just')) {
				if (_p13._2.ctor === 'Nothing') {
					if (_p13._1.ctor === 'Just') {
						return edgeView(
							chopALittleOffBothEnds(
								_opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d(
									{
										ctor: '_Tuple2',
										_0: _jesseilev$graft$View$outgoingPortLocation(_p13._0._0.node),
										_1: _jesseilev$graft$View$incomingPortLocation(_p13._1._0.node)
									})));
					} else {
						break _v5_2;
					}
				} else {
					return edgeView(
						chopALittleOffBothEnds(
							_opensolid$geometry$OpenSolid_Geometry_Types$LineSegment2d(
								{
									ctor: '_Tuple2',
									_0: _jesseilev$graft$View$outgoingPortLocation(_p13._0._0.node),
									_1: _p13._2._0
								})));
				}
			} else {
				break _v5_2;
			}
		} while(false);
		return A2(
			_elm_lang$svg$Svg$g,
			{ctor: '[]'},
			{ctor: '[]'});
	});
var _jesseilev$graft$View$viewNode = F2(
	function (model, node) {
		var outboundEdgePort = _opensolid$geometry$OpenSolid_Geometry_Types$Circle2d(
			{
				radius: _jesseilev$graft$View$controlSize / 10,
				centerPoint: _jesseilev$graft$View$outgoingPortLocation(node)
			});
		var inboundEdgePort = _opensolid$geometry$OpenSolid_Geometry_Types$Circle2d(
			{
				radius: _jesseilev$graft$View$controlSize / 10,
				centerPoint: _jesseilev$graft$View$incomingPortLocation(node)
			});
		var box = _jesseilev$graft$View$nodeControlRect(node);
		var isHoveringOverBox = _elm_lang$core$Native_Utils.eq(
			model.hoverItem,
			_elm_lang$core$Maybe$Just(
				_jesseilev$graft$Types$NodeBox(node.id)));
		var isHoveringOverShape = _elm_lang$core$Native_Utils.eq(
			model.hoverItem,
			_elm_lang$core$Maybe$Just(
				_jesseilev$graft$Types$NodeShape(node.id)));
		var isBeingDraggedTo = function () {
			var _p14 = model.dragAction;
			if ((_p14.ctor === 'Just') && (_p14._0.ctor === 'EdgeChangeEndNode')) {
				return isHoveringOverBox || isHoveringOverShape;
			} else {
				return false;
			}
		}();
		var isSelected = _elm_lang$core$Native_Utils.eq(
			model.selectedItem,
			_elm_lang$core$Maybe$Just(
				_jesseilev$graft$Types$Node(node.id)));
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$opacity(
					isSelected ? '1' : '0.5'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_opensolid$svg$OpenSolid_Svg$polygon2d,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$fill('#ccc'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$cursor(
								_elm_community$maybe_extra$Maybe_Extra$isNothing(model.dragAction) ? '-webkit-grab' : ''),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$stroke('grey'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$strokeWidth(
										(isHoveringOverShape || (isBeingDraggedTo || isSelected)) ? '2px' : '0'),
									_1: {
										ctor: '::',
										_0: A2(
											_zaboco$elm_draggable$Draggable$mouseTrigger,
											_jesseilev$graft$Types$MoveNodeControl(node),
											_jesseilev$graft$Types$DragMsg),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Events$onMouseOver(
												_jesseilev$graft$Types$StartHover(
													_jesseilev$graft$Types$NodeBox(node.id))),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Events$onMouseOut(_jesseilev$graft$Types$StopHover),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					},
					box),
				_1: {
					ctor: '::',
					_0: A2(
						_opensolid$svg$OpenSolid_Svg$placeIn,
						_opensolid$geometry$OpenSolid_Frame2d$at(
							_jesseilev$graft$OpenSolid_Extra$centroid(box)),
						A3(
							_opensolid$svg$OpenSolid_Svg$scaleAbout,
							_opensolid$geometry$OpenSolid_Point2d$origin,
							_jesseilev$graft$View$controlSize * 0.25,
							A2(
								_jesseilev$graft$View$shapeView,
								node,
								{
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$cursor(
										_elm_community$maybe_extra$Maybe_Extra$isNothing(model.dragAction) ? 'pointer' : ''),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Events$onClick(
											_jesseilev$graft$Types$Select(
												_jesseilev$graft$Types$Node(node.id))),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Events$onMouseOver(
												_jesseilev$graft$Types$StartHover(
													_jesseilev$graft$Types$NodeShape(node.id))),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Events$onMouseOut(_jesseilev$graft$Types$StopHover),
												_1: {ctor: '[]'}
											}
										}
									}
								}))),
					_1: {
						ctor: '::',
						_0: A2(
							_opensolid$svg$OpenSolid_Svg$circle2d,
							{
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$fill('grey'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$stroke('grey'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$strokeWidth(
											isBeingDraggedTo ? '20px' : '0'),
										_1: {ctor: '[]'}
									}
								}
							},
							inboundEdgePort),
						_1: {
							ctor: '::',
							_0: A2(
								_opensolid$svg$OpenSolid_Svg$circle2d,
								{
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$fill('grey'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$stroke('grey'),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$strokeWidth(
												_elm_lang$core$Native_Utils.eq(
													model.hoverItem,
													_elm_lang$core$Maybe$Just(
														_jesseilev$graft$Types$OutgoingPort(node.id))) ? '20px' : '0'),
											_1: {
												ctor: '::',
												_0: _elm_lang$svg$Svg_Attributes$cursor('-webkit-grab'),
												_1: {
													ctor: '::',
													_0: A2(
														_zaboco$elm_draggable$Draggable$mouseTrigger,
														A2(
															_jesseilev$graft$Types$EdgeChangeEndNode,
															A3(_elm_community$graph$Graph$Edge, node.id, -1, _jesseilev$graft$View$transformationDefault),
															_jesseilev$graft$OpenSolid_Extra$midRight(box)),
														_jesseilev$graft$Types$DragMsg),
													_1: {
														ctor: '::',
														_0: _elm_lang$svg$Svg_Events$onMouseOver(
															_jesseilev$graft$Types$StartHover(
																_jesseilev$graft$Types$OutgoingPort(node.id))),
														_1: {
															ctor: '::',
															_0: _elm_lang$svg$Svg_Events$onMouseOut(_jesseilev$graft$Types$StopHover),
															_1: {ctor: '[]'}
														}
													}
												}
											}
										}
									}
								},
								outboundEdgePort),
							_1: {ctor: '[]'}
						}
					}
				}
			});
	});
var _jesseilev$graft$View$viewGraph = function (model) {
	var edgeViews = A2(
		_elm_lang$core$List$map,
		_jesseilev$graft$View$viewEdge(model),
		_elm_community$graph$Graph$edges(model.graph));
	var nodeViews = A2(
		_elm_lang$core$List$map,
		_jesseilev$graft$View$viewNode(model),
		_elm_community$graph$Graph$nodes(model.graph));
	return A2(
		_elm_lang$html$Html$section,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$svg$Svg$svg,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$viewBox(
						A2(_jesseilev$graft$View$svgViewBoxString, _jesseilev$graft$View$stageSize, _jesseilev$graft$View$stageSize)),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Events$onMouseUp(_jesseilev$graft$Types$StopDragging),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$style(
								{
									ctor: '::',
									_0: A2(_jesseilev$graft$View_ops['=>'], 'background', '#eee'),
									_1: {
										ctor: '::',
										_0: A2(_jesseilev$graft$View_ops['=>'], 'height', '100%'),
										_1: {
											ctor: '::',
											_0: A2(_jesseilev$graft$View_ops['=>'], 'width', '100%'),
											_1: {
												ctor: '::',
												_0: A2(
													_jesseilev$graft$View_ops['=>'],
													'cursor',
													function () {
														var _p15 = model.dragAction;
														if (_p15.ctor === 'Just') {
															return '-webkit-grabbing';
														} else {
															return 'default';
														}
													}()),
												_1: {ctor: '[]'}
											}
										}
									}
								}),
							_1: {ctor: '[]'}
						}
					}
				},
				A2(_elm_lang$core$Basics_ops['++'], nodeViews, edgeViews)),
			_1: {ctor: '[]'}
		});
};
var _jesseilev$graft$View$viewControls = function (model) {
	return A2(
		_elm_lang$html$Html$section,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				{
					ctor: '::',
					_0: A2(_jesseilev$graft$View_ops['=>'], 'display', 'grid'),
					_1: {
						ctor: '::',
						_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-template-rows', '65% 35%'),
						_1: {ctor: '[]'}
					}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _jesseilev$graft$View$viewGraph(model),
			_1: {
				ctor: '::',
				_0: _jesseilev$graft$View$viewDetailsContainer(model),
				_1: {ctor: '[]'}
			}
		});
};
var _jesseilev$graft$View$viewElement = F3(
	function (cumulativeScale, model, id) {
		var _p16 = {
			ctor: '_Tuple2',
			_0: A2(_elm_community$graph$Graph$get, id, model.graph),
			_1: _elm_lang$core$Native_Utils.cmp((cumulativeScale * model.zoomScale) * _jesseilev$graft$View$stageSize, 4) > -1
		};
		if (((_p16.ctor === '_Tuple2') && (_p16._0.ctor === 'Just')) && (_p16._1 === true)) {
			var _p20 = _p16._0._0;
			var newScale = function (transformation) {
				return transformation.scale * cumulativeScale;
			};
			var viewChild = function (_p17) {
				var _p18 = _p17;
				var _p19 = _p18._1;
				return A2(
					_opensolid$svg$OpenSolid_Svg$translateBy,
					_p19.translation,
					A3(
						_opensolid$svg$OpenSolid_Svg$rotateAround,
						_opensolid$geometry$OpenSolid_Point2d$origin,
						_elm_lang$core$Basics$degrees(_p19.rotation),
						A3(
							_opensolid$svg$OpenSolid_Svg$scaleAbout,
							_opensolid$geometry$OpenSolid_Point2d$origin,
							A3(_elm_lang$core$Basics$clamp, 0, 1, _p19.scale),
							A3(
								_jesseilev$graft$View$viewElement,
								newScale(_p19),
								model,
								_p18._0))));
			};
			var children = A2(
				_elm_lang$core$List$map,
				viewChild,
				_elm_community$intdict$IntDict$toList(_p20.outgoing));
			var parent = A2(
				_jesseilev$graft$View$shapeView,
				_p20.node,
				{ctor: '[]'});
			var element = _p20.node.label;
			return A2(
				_elm_lang$svg$Svg$g,
				{ctor: '[]'},
				{ctor: '::', _0: parent, _1: children});
		} else {
			return A2(
				_elm_lang$svg$Svg$g,
				{ctor: '[]'},
				{ctor: '[]'});
		}
	});
var _jesseilev$graft$View$rescale = function (model) {
	return ((_jesseilev$graft$View$stageSize * 0.5) * 0.6) * model.zoomScale;
};
var _jesseilev$graft$View$viewRootElement = function (model) {
	var halfSize = _jesseilev$graft$View$stageSize / 2;
	var centerVec = _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
		{ctor: '_Tuple2', _0: halfSize, _1: halfSize});
	var trans = A2(
		_elm_lang$core$Debug$log,
		'trans',
		A2(_opensolid$geometry$OpenSolid_Vector2d$difference, model.panOffset, centerVec));
	return A2(
		_opensolid$svg$OpenSolid_Svg$translateBy,
		model.panOffset,
		A3(
			_opensolid$svg$OpenSolid_Svg$scaleAbout,
			A2(_opensolid$geometry$OpenSolid_Point2d$translateBy, trans, _opensolid$geometry$OpenSolid_Point2d$origin),
			_jesseilev$graft$View$rescale(model),
			A2(
				_elm_lang$svg$Svg_Lazy$lazy,
				A2(_jesseilev$graft$View$viewElement, 1, model),
				model.rootId)));
};
var _jesseilev$graft$View$viewStage = function (model) {
	return A2(
		_elm_lang$html$Html$section,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				{
					ctor: '::',
					_0: A2(_jesseilev$graft$View_ops['=>'], 'display', 'grid'),
					_1: {
						ctor: '::',
						_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-template-rows', '1fr 50px'),
						_1: {
							ctor: '::',
							_0: A2(_jesseilev$graft$View_ops['=>'], 'height', '100%'),
							_1: {ctor: '[]'}
						}
					}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$svg$Svg$svg,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$viewBox(
						A2(_jesseilev$graft$View$svgViewBoxString, _jesseilev$graft$View$stageSize, _jesseilev$graft$View$stageSize)),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(
							{
								ctor: '::',
								_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-column', '1 / 3'),
								_1: {
									ctor: '::',
									_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-row', '1 / 3'),
									_1: {
										ctor: '::',
										_0: A2(_jesseilev$graft$View_ops['=>'], 'background', 'grey'),
										_1: {
											ctor: '::',
											_0: A2(_jesseilev$graft$View_ops['=>'], 'cursor', 'move'),
											_1: {
												ctor: '::',
												_0: A2(_jesseilev$graft$View_ops['=>'], 'height', '100%'),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}),
						_1: {
							ctor: '::',
							_0: A2(_zaboco$elm_draggable$Draggable$mouseTrigger, _jesseilev$graft$Types$Pan, _jesseilev$graft$Types$DragMsg),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Events$onMouseUp(_jesseilev$graft$Types$StopDragging),
								_1: {ctor: '[]'}
							}
						}
					}
				},
				{
					ctor: '::',
					_0: A2(_elm_lang$svg$Svg_Lazy$lazy, _jesseilev$graft$View$viewRootElement, model),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(
							{
								ctor: '::',
								_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-row', '2'),
								_1: {
									ctor: '::',
									_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-column', '1'),
									_1: {
										ctor: '::',
										_0: A2(_jesseilev$graft$View_ops['=>'], 'padding', '8px'),
										_1: {
											ctor: '::',
											_0: A2(_jesseilev$graft$View_ops['=>'], 'padding-top', '0'),
											_1: {
												ctor: '::',
												_0: A2(_jesseilev$graft$View_ops['=>'], 'display', 'grid'),
												_1: {
													ctor: '::',
													_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-template-columns', '30px 30px 1fr'),
													_1: {
														ctor: '::',
														_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-gap', '10px'),
														_1: {
															ctor: '::',
															_0: A2(_jesseilev$graft$View_ops['=>'], 'align-items', 'end'),
															_1: {
																ctor: '::',
																_0: A2(_jesseilev$graft$View_ops['=>'], 'justify-items', 'stretch'),
																_1: {ctor: '[]'}
															}
														}
													}
												}
											}
										}
									}
								}
							}),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$button,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Events$onClick(_jesseilev$graft$Types$ZoomIn),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$style(
										{
											ctor: '::',
											_0: A2(_jesseilev$graft$View_ops['=>'], 'font-size', '20px'),
											_1: {
												ctor: '::',
												_0: A2(_jesseilev$graft$View_ops['=>'], 'background', 'rgba(0,0,0,0)'),
												_1: {
													ctor: '::',
													_0: A2(_jesseilev$graft$View_ops['=>'], 'color', 'white'),
													_1: {
														ctor: '::',
														_0: A2(_jesseilev$graft$View_ops['=>'], 'border', '2px solid white'),
														_1: {
															ctor: '::',
															_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-column', '1'),
															_1: {ctor: '[]'}
														}
													}
												}
											}
										}),
									_1: {ctor: '[]'}
								}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('+'),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$button,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Events$onClick(_jesseilev$graft$Types$ZoomOut),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$style(
											{
												ctor: '::',
												_0: A2(_jesseilev$graft$View_ops['=>'], 'font-size', '20px'),
												_1: {
													ctor: '::',
													_0: A2(_jesseilev$graft$View_ops['=>'], 'background', 'rgba(0,0,0,0)'),
													_1: {
														ctor: '::',
														_0: A2(_jesseilev$graft$View_ops['=>'], 'color', 'white'),
														_1: {
															ctor: '::',
															_0: A2(_jesseilev$graft$View_ops['=>'], 'border', '2px solid white'),
															_1: {
																ctor: '::',
																_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-column', '2'),
																_1: {ctor: '[]'}
															}
														}
													}
												}
											}),
										_1: {ctor: '[]'}
									}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text('-'),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					}),
				_1: {ctor: '[]'}
			}
		});
};
var _jesseilev$graft$View$root = function (model) {
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				{
					ctor: '::',
					_0: A2(_jesseilev$graft$View_ops['=>'], 'display', 'grid'),
					_1: {
						ctor: '::',
						_0: A2(_jesseilev$graft$View_ops['=>'], 'grid-template-columns', '1fr 370px'),
						_1: {
							ctor: '::',
							_0: A2(_jesseilev$graft$View_ops['=>'], 'height', '100%'),
							_1: {
								ctor: '::',
								_0: A2(_jesseilev$graft$View_ops['=>'], 'font-family', 'Sans-serif'),
								_1: {
									ctor: '::',
									_0: A2(_jesseilev$graft$View_ops['=>'], 'color', 'grey'),
									_1: {
										ctor: '::',
										_0: A2(_jesseilev$graft$View_ops['=>'], 'background', '#eee'),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(_elm_lang$svg$Svg_Lazy$lazy, _jesseilev$graft$View$viewStage, model),
			_1: {
				ctor: '::',
				_0: _jesseilev$graft$View$viewControls(model),
				_1: {ctor: '[]'}
			}
		});
};

var _jesseilev$graft$Main$exampleGraph = A2(
	_elm_community$graph$Graph$fromNodesAndEdges,
	{
		ctor: '::',
		_0: A2(
			_elm_community$graph$Graph$Node,
			0,
			{
				color: A3(_elm_lang$core$Color$rgb, 100, 0, 200),
				opacity: 0.5,
				shape: _jesseilev$graft$Types$Triangle,
				controlLocation: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
					{ctor: '_Tuple2', _0: 300, _1: 0})
			}),
		_1: {
			ctor: '::',
			_0: A2(
				_elm_community$graph$Graph$Node,
				1,
				{
					color: A3(_elm_lang$core$Color$rgb, 0, 40, 60),
					opacity: 0.25,
					shape: _jesseilev$graft$Types$Square,
					controlLocation: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
						{ctor: '_Tuple2', _0: 120, _1: 120})
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_community$graph$Graph$Node,
					2,
					{
						color: A3(_elm_lang$core$Color$rgb, 200, 100, 0),
						opacity: 0.5,
						shape: _jesseilev$graft$Types$HalfWedge,
						controlLocation: _opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
							{ctor: '_Tuple2', _0: 210, _1: 240})
					}),
				_1: {ctor: '[]'}
			}
		}
	},
	{
		ctor: '::',
		_0: A3(
			_elm_community$graph$Graph$Edge,
			0,
			1,
			{
				translation: _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
					{ctor: '_Tuple2', _0: 0, _1: 0}),
				scale: 0.5,
				rotation: 0
			}),
		_1: {
			ctor: '::',
			_0: A3(
				_elm_community$graph$Graph$Edge,
				1,
				0,
				{
					translation: _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
						{ctor: '_Tuple2', _0: 1, _1: 0}),
					scale: 1 / _elm_lang$core$Basics$sqrt(2),
					rotation: 135
				}),
			_1: {
				ctor: '::',
				_0: A3(
					_elm_community$graph$Graph$Edge,
					1,
					2,
					{
						translation: _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
							{ctor: '_Tuple2', _0: -0.25, _1: 0.25}),
						scale: 0.25,
						rotation: 135
					}),
				_1: {
					ctor: '::',
					_0: A3(
						_elm_community$graph$Graph$Edge,
						2,
						0,
						{
							translation: _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
								{ctor: '_Tuple2', _0: -0.25, _1: -0.25}),
							scale: 0.25,
							rotation: 90
						}),
					_1: {ctor: '[]'}
				}
			}
		}
	});
var _jesseilev$graft$Main$init = {
	rootId: 0,
	graph: _jesseilev$graft$Main$exampleGraph,
	zoomScale: 1,
	panOffset: _opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(
		{ctor: '_Tuple2', _0: _jesseilev$graft$View$stageSize / 2, _1: _jesseilev$graft$View$stageSize / 2}),
	drag: _zaboco$elm_draggable$Draggable$init,
	dragAction: _elm_lang$core$Maybe$Nothing,
	hoverItem: _elm_lang$core$Maybe$Nothing,
	selectedItem: _elm_lang$core$Maybe$Just(
		A2(_jesseilev$graft$Types$Edge, 0, 1))
};
var _jesseilev$graft$Main$subscriptions = function (model) {
	return _elm_lang$core$Platform_Sub$batch(
		{
			ctor: '::',
			_0: _elm_lang$keyboard$Keyboard$presses(
				function (code) {
					var _p0 = A2(
						_elm_lang$core$Debug$log,
						'key code',
						_ohanhi$keyboard_extra$Keyboard_Extra$fromCode(code));
					if (_p0.ctor === 'CharZ') {
						return _jesseilev$graft$Types$Delete;
					} else {
						return _jesseilev$graft$Types$NoOp;
					}
				}),
			_1: {
				ctor: '::',
				_0: A2(_zaboco$elm_draggable$Draggable$subscriptions, _jesseilev$graft$Types$DragMsg, model.drag),
				_1: {ctor: '[]'}
			}
		});
};
var _jesseilev$graft$Main$moveNodeControl = F3(
	function (vec, node, model) {
		var updateElement = function (element) {
			return _elm_lang$core$Native_Utils.update(
				element,
				{
					controlLocation: A2(_opensolid$geometry$OpenSolid_Point2d$translateBy, vec, element.controlLocation)
				});
		};
		var updater = function (node) {
			return _elm_lang$core$Native_Utils.update(
				node,
				{
					label: updateElement(node.label)
				});
		};
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				graph: A3(_jesseilev$graft$Graph_Extra$updateNode, node.id, updater, model.graph)
			});
	});
var _jesseilev$graft$Main$cleanupTempNodesAndEdges = function (model) {
	var removeNeighborlessNodes = _jesseilev$graft$Graph_Extra$updateNodes(
		_elm_lang$core$List$filter(
			function (n) {
				return _elm_lang$core$Native_Utils.cmp(
					A2(_jesseilev$graft$Graph_Extra$neighborCount, model.graph, n),
					0) > 0;
			}));
	var removeHangingEdges = _jesseilev$graft$Graph_Extra$updateEdges(
		_elm_lang$core$List$filter(
			function (_p1) {
				return A2(
					F2(
						function (x, y) {
							return !_elm_lang$core$Native_Utils.eq(x, y);
						}),
					-1,
					function (_) {
						return _.to;
					}(_p1));
			}));
	var cleanupGraph = function (_p2) {
		return removeHangingEdges(
			removeNeighborlessNodes(_p2));
	};
	return _elm_lang$core$Native_Utils.update(
		model,
		{
			graph: cleanupGraph(model.graph)
		});
};
var _jesseilev$graft$Main$nextId = function (_p3) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		0,
		A2(
			_elm_lang$core$Maybe$map,
			function (_p4) {
				return A2(
					F2(
						function (x, y) {
							return x + y;
						}),
					1,
					_elm_lang$core$Tuple$second(_p4));
			},
			_elm_community$graph$Graph$nodeIdRange(_p3)));
};
var _jesseilev$graft$Main$newNode = function (model) {
	return A2(
		_elm_community$graph$Graph$Node,
		_jesseilev$graft$Main$nextId(model.graph),
		A4(
			_jesseilev$graft$Types$Element,
			_elm_lang$core$Color$white,
			0.5,
			_jesseilev$graft$Types$Circle,
			_opensolid$geometry$OpenSolid_Geometry_Types$Point2d(
				{ctor: '_Tuple2', _0: 300, _1: 400})));
};
var _jesseilev$graft$Main$newNodeContext = function (model) {
	return A3(
		_elm_community$graph$Graph$NodeContext,
		_jesseilev$graft$Main$newNode(model),
		_elm_community$intdict$IntDict$empty,
		_elm_community$intdict$IntDict$empty);
};
var _jesseilev$graft$Main$updateGraph = _arturopala$elm_monocle$Monocle_Lens$modify(_jesseilev$graft$Types$modelLensGraph);
var _jesseilev$graft$Main$updateEdge = F2(
	function (from, to) {
		return function (_p5) {
			return _jesseilev$graft$Main$updateGraph(
				A3(_jesseilev$graft$Graph_Extra$updateEdge, from, to, _p5));
		};
	});
var _jesseilev$graft$Main$updateNode = function (nodeId) {
	return function (_p6) {
		return _jesseilev$graft$Main$updateGraph(
			A2(_jesseilev$graft$Graph_Extra$updateNode, nodeId, _p6));
	};
};
var _jesseilev$graft$Main$dragConfig = _zaboco$elm_draggable$Draggable$customConfig(
	{
		ctor: '::',
		_0: _zaboco$elm_draggable$Draggable_Events$onDragBy(
			function (_p7) {
				return _jesseilev$graft$Types$OnDragBy(
					_opensolid$geometry$OpenSolid_Geometry_Types$Vector2d(_p7));
			}),
		_1: {
			ctor: '::',
			_0: _zaboco$elm_draggable$Draggable_Events$onDragStart(_jesseilev$graft$Types$StartDragging),
			_1: {ctor: '[]'}
		}
	});
var _jesseilev$graft$Main$update = F2(
	function (msg, model) {
		var _p8 = msg;
		switch (_p8.ctor) {
			case 'ZoomIn':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					_elm_lang$core$Native_Utils.update(
						model,
						{zoomScale: model.zoomScale * 1.1}),
					{ctor: '[]'});
			case 'ZoomOut':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					_elm_lang$core$Native_Utils.update(
						model,
						{zoomScale: model.zoomScale / 1.1}),
					{ctor: '[]'});
			case 'StartHover':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					_elm_lang$core$Native_Utils.update(
						model,
						{
							hoverItem: _elm_lang$core$Maybe$Just(_p8._0)
						}),
					{ctor: '[]'});
			case 'StopHover':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					_elm_lang$core$Native_Utils.update(
						model,
						{hoverItem: _elm_lang$core$Maybe$Nothing}),
					{ctor: '[]'});
			case 'Select':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					_elm_lang$core$Native_Utils.update(
						model,
						{
							selectedItem: _elm_lang$core$Maybe$Just(_p8._0)
						}),
					{ctor: '[]'});
			case 'Deselect':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					_elm_lang$core$Native_Utils.update(
						model,
						{selectedItem: _elm_lang$core$Maybe$Nothing}),
					{ctor: '[]'});
			case 'StartDragging':
				var _p13 = _p8._0;
				var insertTemporaryNewNodeAndEdge = function (newEdge) {
					return function (_p9) {
						return A2(
							_jesseilev$graft$Graph_Extra$insertEdge,
							newEdge,
							A2(
								_elm_community$graph$Graph$insert,
								_jesseilev$graft$Main$newNodeContext(model),
								_p9));
					};
				};
				var insertTempsIfChangingEdgeEnd = function () {
					var _p10 = _p13;
					if (_p10.ctor === 'EdgeChangeEndNode') {
						var _p12 = _p10._0;
						return function (_p11) {
							return function (m) {
								return _elm_lang$core$Native_Utils.update(
									m,
									{
										selectedItem: _elm_lang$core$Maybe$Just(
											A2(_jesseilev$graft$Types$Edge, _p12.from, _p12.to))
									});
							}(
								A2(
									_jesseilev$graft$Main$updateGraph,
									insertTemporaryNewNodeAndEdge(_p12),
									_p11));
						};
					} else {
						return _elm_lang$core$Basics$identity;
					}
				}();
				return A3(
					_elm_lang$core$Basics$flip,
					F2(
						function (v0, v1) {
							return {ctor: '_Tuple2', _0: v0, _1: v1};
						}),
					_elm_lang$core$Platform_Cmd$none,
					insertTempsIfChangingEdgeEnd(
						_elm_lang$core$Native_Utils.update(
							model,
							{
								dragAction: _elm_lang$core$Maybe$Just(_p13)
							})));
			case 'StopDragging':
				var destinationNodeId = A2(_elm_lang$core$Maybe$andThen, _jesseilev$graft$Types$getHoverableNodeId, model.hoverItem);
				var updatedModel = function () {
					var _p14 = {ctor: '_Tuple2', _0: model.dragAction, _1: destinationNodeId};
					if ((((_p14.ctor === '_Tuple2') && (_p14._0.ctor === 'Just')) && (_p14._0._0.ctor === 'EdgeChangeEndNode')) && (_p14._1.ctor === 'Just')) {
						var _p15 = _p14._0._0._0;
						var updatedEdge = _elm_lang$core$Native_Utils.update(
							_p15,
							{to: _p14._1._0});
						var replaceOldEdge = A2(
							_elm_community$list_extra$List_Extra$replaceIf,
							_jesseilev$graft$Graph_Extra$edgeEquals(_p15),
							updatedEdge);
						return _elm_lang$core$Native_Utils.update(
							model,
							{
								graph: A2(_jesseilev$graft$Graph_Extra$updateEdges, replaceOldEdge, model.graph)
							});
					} else {
						return model;
					}
				}();
				return A3(
					_elm_lang$core$Basics$flip,
					F2(
						function (v0, v1) {
							return {ctor: '_Tuple2', _0: v0, _1: v1};
						}),
					_elm_lang$core$Platform_Cmd$none,
					_jesseilev$graft$Main$cleanupTempNodesAndEdges(
						_elm_lang$core$Native_Utils.update(
							updatedModel,
							{dragAction: _elm_lang$core$Maybe$Nothing})));
			case 'OnDragBy':
				var _p17 = _p8._0;
				var _p16 = model.dragAction;
				if (_p16.ctor === 'Just') {
					switch (_p16._0.ctor) {
						case 'MoveNodeControl':
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								A3(_jesseilev$graft$Main$moveNodeControl, _p17, _p16._0._0, model),
								{ctor: '[]'});
						case 'EdgeChangeEndNode':
							var newEndpoint = A2(_opensolid$geometry$OpenSolid_Point2d$translateBy, _p17, _p16._0._1);
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								_elm_lang$core$Native_Utils.update(
									model,
									{
										dragAction: _elm_lang$core$Maybe$Just(
											A2(_jesseilev$graft$Types$EdgeChangeEndNode, _p16._0._0, newEndpoint))
									}),
								{ctor: '[]'});
						default:
							var scaledVec = A2(
								_opensolid$geometry$OpenSolid_Vector2d$scaleBy,
								1 / _jesseilev$graft$View$rescale(model),
								_opensolid$geometry$OpenSolid_Vector2d$flip(_p17));
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								_elm_lang$core$Native_Utils.update(
									model,
									{
										panOffset: A2(_opensolid$geometry$OpenSolid_Vector2d$sum, scaledVec, model.panOffset)
									}),
								{ctor: '[]'});
					}
				} else {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						model,
						{ctor: '[]'});
				}
			case 'DragMsg':
				return A3(_zaboco$elm_draggable$Draggable$update, _jesseilev$graft$Main$dragConfig, _p8._0, model);
			case 'Delete':
				var _p18 = model.selectedItem;
				if ((_p18.ctor === 'Just') && (_p18._0.ctor === 'Node')) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						_elm_lang$core$Native_Utils.update(
							model,
							{
								graph: A2(_elm_community$graph$Graph$remove, _p18._0._0, model.graph)
							}),
						{ctor: '[]'});
				} else {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						model,
						{ctor: '[]'});
				}
			case 'ChangeColor':
				var color = function (_p19) {
					return A2(
						_elm_lang$core$Result$withDefault,
						_elm_lang$core$Color$black,
						_eskimoblood$elm_color_extra$Color_Convert$hexToColor(_p19));
				}(_p8._1);
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					A3(
						_jesseilev$graft$Main$updateNode,
						_p8._0.id,
						_jesseilev$graft$Types$nodeLensColor.set(color),
						model),
					{ctor: '[]'});
			case 'ChangeOpacity':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					A3(
						_jesseilev$graft$Main$updateNode,
						_p8._0.id,
						_jesseilev$graft$Types$nodeLensOpacity.set(_p8._1),
						model),
					{ctor: '[]'});
			case 'ChangeShape':
				var shape = function (_p20) {
					return A2(
						_elm_lang$core$Result$withDefault,
						_jesseilev$graft$Types$Square,
						_jesseilev$graft$Types$shapeFromString(_p20));
				}(_p8._1);
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					A3(
						_jesseilev$graft$Main$updateNode,
						_p8._0.id,
						_jesseilev$graft$Types$nodeLensShape.set(shape),
						model),
					{ctor: '[]'});
			case 'ChangeScale':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					A4(
						_jesseilev$graft$Main$updateEdge,
						_p8._0.from,
						_p8._0.to,
						_jesseilev$graft$Types$edgeLensScale.set(_p8._1),
						model),
					{ctor: '[]'});
			case 'ChangeRotation':
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					A4(
						_jesseilev$graft$Main$updateEdge,
						_p8._0.from,
						_p8._0.to,
						_jesseilev$graft$Types$edgeLensRotation.set(_p8._1),
						model),
					{ctor: '[]'});
			case 'TranslationX':
				var setX = A2(
					_arturopala$elm_monocle$Monocle_Lens$modify,
					_jesseilev$graft$Types$edgeLensTranslation,
					_jesseilev$graft$OpenSolid_Vector2d_Extra$setX(_p8._1));
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					A4(_jesseilev$graft$Main$updateEdge, _p8._0.from, _p8._0.to, setX, model),
					{ctor: '[]'});
			case 'TranslationY':
				var setY = A2(
					_arturopala$elm_monocle$Monocle_Lens$modify,
					_jesseilev$graft$Types$edgeLensTranslation,
					_jesseilev$graft$OpenSolid_Vector2d_Extra$setY(_p8._1));
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					A4(_jesseilev$graft$Main$updateEdge, _p8._0.from, _p8._0.to, setY, model),
					{ctor: '[]'});
			default:
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					model,
					{ctor: '[]'});
		}
	});
var _jesseilev$graft$Main$main = _elm_lang$html$Html$program(
	{
		init: A2(
			_elm_lang$core$Platform_Cmd_ops['!'],
			_jesseilev$graft$Main$init,
			{ctor: '[]'}),
		update: _jesseilev$graft$Main$update,
		subscriptions: _jesseilev$graft$Main$subscriptions,
		view: _jesseilev$graft$View$root
	})();

var Elm = {};
Elm['Main'] = Elm['Main'] || {};
if (typeof _jesseilev$graft$Main$main !== 'undefined') {
    _jesseilev$graft$Main$main(Elm['Main'], 'Main', undefined);
}

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);

