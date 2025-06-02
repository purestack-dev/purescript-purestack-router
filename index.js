(() => {
  // output/Bun.Server/foreign.js
  function serve(f) {
    return function() {
      return Bun.serve({
        fetch(req) {
          return f(req)();
        }
      });
    };
  }

  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i = 0; i < l; i++) {
        var f = fs[i];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b) {
      return function(a) {
        return f(a)(b);
      };
    };
  };
  var $$const = function(a) {
    return function(v) {
      return a;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var liftA1 = function(dictApplicative) {
    var apply3 = apply(dictApplicative.Apply0());
    var pure13 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply3(pure13(f))(a);
      };
    };
  };

  // output/Data.Argonaut.Core/foreign.js
  function id(x) {
    return x;
  }

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqStringImpl = refEq;
  var eqArrayImpl = function(f) {
    return function(xs) {
      return function(ys) {
        if (xs.length !== ys.length) return false;
        for (var i = 0; i < xs.length; i++) {
          if (!f(xs[i])(ys[i])) return false;
        }
        return true;
      };
    };
  };

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label) {
    return function(rec) {
      return rec[label];
    };
  };
  var unsafeDelete = function(label) {
    return function(rec) {
      var copy = {};
      for (var key in rec) {
        if (key !== label && {}.hasOwnProperty.call(rec, key)) {
          copy[key] = rec[key];
        }
      }
      return copy;
    };
  };

  // output/Data.Eq/index.js
  var eqString = {
    eq: eqStringImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };
  var eqArray = function(dictEq) {
    return {
      eq: eqArrayImpl(eq(dictEq))
    };
  };

  // output/Data.Semigroup/foreign.js
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0) return ys;
      if (ys.length === 0) return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Data.Bounded/foreign.js
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Maybe/index.js
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };
  var applicativeMaybe = /* @__PURE__ */ function() {
    return {
      pure: Just.create,
      Apply0: function() {
        return applyMaybe;
      }
    };
  }();
  var altMaybe = {
    alt: function(v) {
      return function(v1) {
        if (v instanceof Nothing) {
          return v1;
        }
        ;
        return v;
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var plusMaybe = /* @__PURE__ */ function() {
    return {
      empty: Nothing.value,
      Alt0: function() {
        return altMaybe;
      }
    };
  }();

  // output/Foreign.Object/foreign.js
  function _copyST(m) {
    return function() {
      var r = {};
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r[k] = m[k];
        }
      }
      return r;
    };
  }
  var empty = {};
  function runST(f) {
    return f();
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Control.Bind/foreign.js
  var arrayBind = typeof Array.prototype.flatMap === "function" ? function(arr) {
    return function(f) {
      return arr.flatMap(f);
    };
  } : function(arr) {
    return function(f) {
      var result = [];
      var l = arr.length;
      for (var i = 0; i < l; i++) {
        var xs = f(arr[i]);
        var k = xs.length;
        for (var j = 0; j < k; j++) {
          result.push(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Bind/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var discard = function(dict) {
    return dict.discard;
  };
  var bindArray = {
    bind: arrayBind,
    Apply0: function() {
      return applyArray;
    }
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };
  var join = function(dictBind) {
    var bind1 = bind(dictBind);
    return function(m) {
      return bind1(m)(identity2);
    };
  };

  // output/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a) {
      return function() {
        return f(a());
      };
    };
  };
  var pure_ = function(a) {
    return function() {
      return a;
    };
  };
  var bind_ = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };
  function newSTRef(val) {
    return function() {
      return { value: val };
    };
  }
  var read = function(ref) {
    return function() {
      return ref.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref) {
      return function() {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };
  var write = function(a) {
    return function(ref) {
      return function() {
        return ref.value = a;
      };
    };
  };

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind3 = bind(dictMonad.Bind1());
    var pure4 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind3(f)(function(f$prime) {
          return bind3(a)(function(a$prime) {
            return pure4(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();

  // output/Data.Monoid/index.js
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name2, moduleName, init3) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init3();
      state2 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });

  // output/Control.Monad.ST.Internal/index.js
  var $runtime_lazy2 = function(name2, moduleName, init3) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2) return val;
      if (state2 === 1) throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init3();
      state2 = 2;
      return val;
    };
  };
  var modify$prime = modifyImpl;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var functorST = {
    map: map_
  };
  var monadST = {
    Applicative0: function() {
      return applicativeST;
    },
    Bind1: function() {
      return bindST;
    }
  };
  var bindST = {
    bind: bind_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var applicativeST = {
    pure: pure_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var $lazy_applyST = /* @__PURE__ */ $runtime_lazy2("applyST", "Control.Monad.ST.Internal", function() {
    return {
      apply: ap(monadST),
      Functor0: function() {
        return functorST;
      }
    };
  });
  var applyST = /* @__PURE__ */ $lazy_applyST(47);

  // output/Data.Array/foreign.js
  var replicateFill = function(count, value) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value);
  };
  var replicatePolyfill = function(count, value) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count; i++) {
      result[n++] = value;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var length = function(xs) {
    return xs.length;
  };
  var unconsImpl = function(empty4, next2, xs) {
    return xs.length === 0 ? empty4({}) : next2(xs[0])(xs.slice(1));
  };
  var indexImpl = function(just, nothing, xs, i) {
    return i < 0 || i >= xs.length ? nothing : just(xs[i]);
  };
  var filterImpl = function(f, xs) {
    return xs.filter(f);
  };
  var partitionImpl = function(f, xs) {
    var yes = [];
    var no = [];
    for (var i = 0; i < xs.length; i++) {
      var x = xs[i];
      if (f(x))
        yes.push(x);
      else
        no.push(x);
    }
    return { yes, no };
  };

  // output/Data.Array.ST/foreign.js
  function newSTArray() {
    return [];
  }
  function unsafeFreezeThawImpl(xs) {
    return xs;
  }
  var unsafeFreezeImpl = unsafeFreezeThawImpl;
  var pushImpl = function(a, xs) {
    return xs.push(a);
  };

  // output/Control.Monad.ST.Uncurried/foreign.js
  var runSTFn1 = function runSTFn12(fn) {
    return function(a) {
      return function() {
        return fn(a);
      };
    };
  };
  var runSTFn2 = function runSTFn22(fn) {
    return function(a) {
      return function(b) {
        return function() {
          return fn(a, b);
        };
      };
    };
  };

  // output/Data.Array.ST/index.js
  var unsafeFreeze = /* @__PURE__ */ runSTFn1(unsafeFreezeImpl);
  var push = /* @__PURE__ */ runSTFn2(pushImpl);

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b) {
    return !b;
  };

  // output/Data.HeytingAlgebra/index.js
  var not = function(dict) {
    return dict.not;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a) {
      return function(b) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };

  // output/Data.Array.ST.Iterator/index.js
  var map3 = /* @__PURE__ */ map(functorST);
  var not2 = /* @__PURE__ */ not(heytingAlgebraBoolean);
  var $$void2 = /* @__PURE__ */ $$void(functorST);
  var Iterator = /* @__PURE__ */ function() {
    function Iterator2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Iterator2.create = function(value0) {
      return function(value1) {
        return new Iterator2(value0, value1);
      };
    };
    return Iterator2;
  }();
  var next = function(v) {
    return function __do2() {
      var i = read(v.value1)();
      modify(function(v1) {
        return v1 + 1 | 0;
      })(v.value1)();
      return v.value0(i);
    };
  };
  var iterator = function(f) {
    return map3(Iterator.create(f))(newSTRef(0));
  };
  var iterate = function(iter) {
    return function(f) {
      return function __do2() {
        var $$break = newSTRef(false)();
        while (map3(not2)(read($$break))()) {
          (function __do3() {
            var mx = next(iter)();
            if (mx instanceof Just) {
              return f(mx.value0)();
            }
            ;
            if (mx instanceof Nothing) {
              return $$void2(write(true)($$break))();
            }
            ;
            throw new Error("Failed pattern match at Data.Array.ST.Iterator (line 42, column 5 - line 44, column 47): " + [mx.constructor.name]);
          })();
        }
        ;
        return {};
      };
    };
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output/Control.Plus/index.js
  var empty2 = function(dict) {
    return dict.empty;
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var wrap = function() {
    return coerce2;
  };

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr2 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append3 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr2(function(x) {
          return function(acc) {
            return append3(f(x))(acc);
          };
        })(mempty2);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };

  // output/Data.Function.Uncurried/foreign.js
  var runFn2 = function(fn) {
    return function(a) {
      return function(b) {
        return fn(a, b);
      };
    };
  };
  var runFn3 = function(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return fn(a, b, c);
        };
      };
    };
  };
  var runFn4 = function(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return fn(a, b, c, d);
          };
        };
      };
    };
  };

  // output/Data.Array/index.js
  var uncons = /* @__PURE__ */ function() {
    return runFn3(unconsImpl)($$const(Nothing.value))(function(x) {
      return function(xs) {
        return new Just({
          head: x,
          tail: xs
        });
      };
    });
  }();
  var singleton2 = function(a) {
    return [a];
  };
  var partition = /* @__PURE__ */ runFn2(partitionImpl);
  var $$null = function(xs) {
    return length(xs) === 0;
  };
  var index = /* @__PURE__ */ function() {
    return runFn4(indexImpl)(Just.create)(Nothing.value);
  }();
  var filter = /* @__PURE__ */ runFn2(filterImpl);
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap(function() {
      var $189 = maybe([])(singleton2);
      return function($190) {
        return $189(f($190));
      };
    }());
  };

  // output/Foreign.Object.ST/foreign.js
  function poke2(k) {
    return function(v) {
      return function(m) {
        return function() {
          m[k] = v;
          return m;
        };
      };
    };
  }

  // output/Foreign.Object/index.js
  var thawST = _copyST;
  var mutate = function(f) {
    return function(m) {
      return runST(function __do2() {
        var s = thawST(m)();
        f(s)();
        return s;
      });
    };
  };
  var insert = function(k) {
    return function(v) {
      return mutate(poke2(k)(v));
    };
  };

  // output/Data.Int/foreign.js
  var toNumber = function(n) {
    return n;
  };

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";

  // output/Data.String.Common/foreign.js
  var split = function(sep) {
    return function(s) {
      return s.split(sep);
    };
  };

  // output/Data.String.Common/index.js
  var $$null2 = function(s) {
    return s === "";
  };

  // output/Data.Argonaut.Encode.Encoders/index.js
  var encodeNumber = id;
  var encodeInt = function($53) {
    return id(toNumber($53));
  };

  // output/Record/index.js
  var get = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function(l) {
        return function(r) {
          return unsafeGet(reflectSymbol2(l))(r);
        };
      };
    };
  };
  var $$delete3 = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function() {
        return function(l) {
          return function(r) {
            return unsafeDelete(reflectSymbol2(l))(r);
          };
        };
      };
    };
  };

  // output/Data.Argonaut.Encode.Class/index.js
  var gEncodeJsonNil = {
    gEncodeJson: function(v) {
      return function(v1) {
        return empty;
      };
    }
  };
  var gEncodeJson = function(dict) {
    return dict.gEncodeJson;
  };
  var encodeRecord = function(dictGEncodeJson) {
    var gEncodeJson1 = gEncodeJson(dictGEncodeJson);
    return function() {
      return {
        encodeJson: function(rec) {
          return id(gEncodeJson1(rec)($$Proxy.value));
        }
      };
    };
  };
  var encodeJsonJNumber = {
    encodeJson: encodeNumber
  };
  var encodeJsonInt = {
    encodeJson: encodeInt
  };
  var encodeJson = function(dict) {
    return dict.encodeJson;
  };
  var gEncodeJsonCons = function(dictEncodeJson) {
    var encodeJson1 = encodeJson(dictEncodeJson);
    return function(dictGEncodeJson) {
      var gEncodeJson1 = gEncodeJson(dictGEncodeJson);
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        var get2 = get(dictIsSymbol)();
        return function() {
          return {
            gEncodeJson: function(row) {
              return function(v) {
                return insert(reflectSymbol2($$Proxy.value))(encodeJson1(get2($$Proxy.value)(row)))(gEncodeJson1(row)($$Proxy.value));
              };
            }
          };
        };
      };
    };
  };

  // output/Effect.Console/foreign.js
  var log2 = function(s) {
    return function() {
      console.log(s);
    };
  };

  // output/Bun.Request/foreign.js
  function method(req) {
    return req.method;
  }
  function url(req) {
    return req.url;
  }

  // output/Bun.Response/foreign.js
  function json2(j) {
    return function(options) {
      return Response.json(j, options);
    };
  }
  function string(s) {
    return function(options) {
      return new Response(s, options);
    };
  }

  // output/Data.URL/foreign.js
  var fromStringImpl2 = (s) => {
    try {
      return new URL(s);
    } catch {
      return null;
    }
  };
  var pathnameImpl = (u) => u.pathname;

  // output/Data.Compactable/index.js
  var $$void3 = /* @__PURE__ */ $$void(functorST);
  var pure1 = /* @__PURE__ */ pure(applicativeST);
  var apply2 = /* @__PURE__ */ apply(applyST);
  var map4 = /* @__PURE__ */ map(functorST);
  var compactableMaybe = {
    compact: /* @__PURE__ */ join(bindMaybe),
    separate: function(v) {
      if (v instanceof Nothing) {
        return {
          left: Nothing.value,
          right: Nothing.value
        };
      }
      ;
      if (v instanceof Just) {
        if (v.value0 instanceof Left) {
          return {
            left: new Just(v.value0.value0),
            right: Nothing.value
          };
        }
        ;
        if (v.value0 instanceof Right) {
          return {
            left: Nothing.value,
            right: new Just(v.value0.value0)
          };
        }
        ;
        throw new Error("Failed pattern match at Data.Compactable (line 91, column 23 - line 93, column 48): " + [v.value0.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Compactable (line 87, column 1 - line 93, column 48): " + [v.constructor.name]);
    }
  };
  var compactableArray = {
    compact: function(xs) {
      return function __do2() {
        var result = newSTArray();
        var iter = iterator(function(v) {
          return index(xs)(v);
        })();
        iterate(iter)(function($108) {
          return $$void3(function(v) {
            if (v instanceof Nothing) {
              return pure1(0);
            }
            ;
            if (v instanceof Just) {
              return push(v.value0)(result);
            }
            ;
            throw new Error("Failed pattern match at Data.Compactable (line 111, column 34 - line 113, column 35): " + [v.constructor.name]);
          }($108));
        })();
        return unsafeFreeze(result)();
      }();
    },
    separate: function(xs) {
      return function __do2() {
        var ls = newSTArray();
        var rs = newSTArray();
        var iter = iterator(function(v) {
          return index(xs)(v);
        })();
        iterate(iter)(function($109) {
          return $$void3(function(v) {
            if (v instanceof Left) {
              return push(v.value0)(ls);
            }
            ;
            if (v instanceof Right) {
              return push(v.value0)(rs);
            }
            ;
            throw new Error("Failed pattern match at Data.Compactable (line 122, column 34 - line 124, column 31): " + [v.constructor.name]);
          }($109));
        })();
        return apply2(map4(function(v) {
          return function(v1) {
            return {
              left: v,
              right: v1
            };
          };
        })(unsafeFreeze(ls)))(unsafeFreeze(rs))();
      }();
    }
  };

  // output/Data.Filterable/index.js
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var foldl2 = /* @__PURE__ */ foldl(foldableArray);
  var partitionMap = function(dict) {
    return dict.partitionMap;
  };
  var maybeBool = function(p) {
    return function(x) {
      var $66 = p(x);
      if ($66) {
        return new Just(x);
      }
      ;
      return Nothing.value;
    };
  };
  var filterableArray = {
    partitionMap: function(p) {
      var go = function(acc) {
        return function(x) {
          var v = p(x);
          if (v instanceof Left) {
            return {
              right: acc.right,
              left: append2(acc.left)([v.value0])
            };
          }
          ;
          if (v instanceof Right) {
            return {
              left: acc.left,
              right: append2(acc.right)([v.value0])
            };
          }
          ;
          throw new Error("Failed pattern match at Data.Filterable (line 149, column 16 - line 151, column 50): " + [v.constructor.name]);
        };
      };
      return foldl2(go)({
        left: [],
        right: []
      });
    },
    partition,
    filterMap: mapMaybe,
    filter,
    Compactable0: function() {
      return compactableArray;
    },
    Functor1: function() {
      return functorArray;
    }
  };
  var filterMap = function(dict) {
    return dict.filterMap;
  };
  var filterDefault = function(dictFilterable) {
    var $121 = filterMap(dictFilterable);
    return function($122) {
      return $121(maybeBool($122));
    };
  };
  var filter3 = function(dict) {
    return dict.filter;
  };
  var eitherBool = function(p) {
    return function(x) {
      var $84 = p(x);
      if ($84) {
        return new Right(x);
      }
      ;
      return new Left(x);
    };
  };
  var partitionDefault = function(dictFilterable) {
    var partitionMap1 = partitionMap(dictFilterable);
    return function(p) {
      return function(xs) {
        var o = partitionMap1(eitherBool(p))(xs);
        return {
          no: o.left,
          yes: o.right
        };
      };
    };
  };
  var filterableMaybe = {
    partitionMap: function(v) {
      return function(v1) {
        if (v1 instanceof Nothing) {
          return {
            left: Nothing.value,
            right: Nothing.value
          };
        }
        ;
        if (v1 instanceof Just) {
          var v2 = v(v1.value0);
          if (v2 instanceof Left) {
            return {
              left: new Just(v2.value0),
              right: Nothing.value
            };
          }
          ;
          if (v2 instanceof Right) {
            return {
              left: Nothing.value,
              right: new Just(v2.value0)
            };
          }
          ;
          throw new Error("Failed pattern match at Data.Filterable (line 161, column 29 - line 163, column 48): " + [v2.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Filterable (line 159, column 1 - line 169, column 29): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    partition: function(p) {
      return partitionDefault(filterableMaybe)(p);
    },
    filterMap: /* @__PURE__ */ bindFlipped(bindMaybe),
    filter: function(p) {
      return filterDefault(filterableMaybe)(p);
    },
    Compactable0: function() {
      return compactableMaybe;
    },
    Functor1: function() {
      return functorMaybe;
    }
  };

  // output/Data.Nullable/foreign.js
  function nullable(a, r, f) {
    return a == null ? r : f(a);
  }

  // output/Data.Nullable/index.js
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Data.String.Utils/foreign.js
  function startsWithImpl(searchString, s) {
    return s.startsWith(searchString);
  }

  // output/Data.String.Utils/index.js
  var startsWith = function(searchString) {
    return function(s) {
      return startsWithImpl(searchString, s);
    };
  };

  // output/Foreign/foreign.js
  var isArray = Array.isArray || function(value) {
    return Object.prototype.toString.call(value) === "[object Array]";
  };

  // output/Data.URL/index.js
  var filter4 = /* @__PURE__ */ filter3(filterableMaybe);
  var wrap2 = /* @__PURE__ */ wrap();
  var filter1 = /* @__PURE__ */ filter3(filterableArray);
  var PathEmpty = /* @__PURE__ */ function() {
    function PathEmpty2() {
    }
    ;
    PathEmpty2.value = new PathEmpty2();
    return PathEmpty2;
  }();
  var PathAbsolute = /* @__PURE__ */ function() {
    function PathAbsolute2(value0) {
      this.value0 = value0;
    }
    ;
    PathAbsolute2.create = function(value0) {
      return new PathAbsolute2(value0);
    };
    return PathAbsolute2;
  }();
  var PathRelative = /* @__PURE__ */ function() {
    function PathRelative2(value0) {
      this.value0 = value0;
    }
    ;
    PathRelative2.create = function(value0) {
      return new PathRelative2(value0);
    };
    return PathRelative2;
  }();
  var pathFromString = function(s) {
    var segments = function() {
      var $235 = filter1(function($238) {
        return !$$null2($238);
      });
      var $236 = split(wrap2("/"));
      return function($237) {
        return $235($236($237));
      };
    }();
    return maybe(PathEmpty.value)(function() {
      var $203 = startsWith("/")(s);
      if ($203) {
        return PathAbsolute.create;
      }
      ;
      return PathRelative.create;
    }())(filter4(function($239) {
      return !$$null($239);
    })(new Just(segments(s))));
  };
  var path = function($240) {
    return pathFromString(pathnameImpl($240));
  };
  var fromString2 = function($255) {
    return toMaybe(fromStringImpl2($255));
  };

  // output/PureStack.Router/index.js
  var bind2 = /* @__PURE__ */ bind(bindMaybe);
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var discard2 = /* @__PURE__ */ discard(discardUnit)(bindMaybe);
  var eq12 = /* @__PURE__ */ eq(/* @__PURE__ */ eqArray(eqString));
  var pure12 = /* @__PURE__ */ pure(applicativeMaybe);
  var toResponseRecord = function(dictEncodeJson) {
    var encodeJson2 = encodeJson(dictEncodeJson);
    return {
      toResponse: function(rec) {
        return function(opts) {
          return json2(encodeJson2(rec))(opts);
        };
      }
    };
  };
  var toResponse = function(dict) {
    return dict.toResponse;
  };
  var runAPI = function(dict) {
    return dict.runAPI;
  };
  var run3 = function() {
    return function(dictRunAPI) {
      return runAPI(dictRunAPI);
    };
  };
  var parseRoute = function(dict) {
    return dict.parseRoute;
  };
  var notFound = /* @__PURE__ */ string("")({
    status: 404,
    statusText: "Not Found",
    headers: []
  });
  var runAPINilRow = {
    runAPI: function(v) {
      return function(v1) {
        return pure2(notFound);
      };
    }
  };
  var internalServerError = /* @__PURE__ */ string("")({
    status: 500,
    statusText: "Internal Server Error",
    headers: []
  });
  var runAPICons = function(dictParseRoute) {
    var parseRoute1 = parseRoute(dictParseRoute);
    return function() {
      return function(dictRunAPI) {
        var runAPI1 = runAPI(dictRunAPI);
        return function(dictIsSymbol) {
          var $$delete4 = $$delete3(dictIsSymbol)()();
          var get2 = get(dictIsSymbol)();
          return function() {
            return function() {
              return {
                runAPI: function(handlers) {
                  return function(req) {
                    var v = fromString2(url(req));
                    if (v instanceof Nothing) {
                      return pure2(internalServerError);
                    }
                    ;
                    if (v instanceof Just) {
                      var verb1 = method(req);
                      var path2 = function() {
                        var v12 = path(v.value0);
                        if (v12 instanceof PathEmpty) {
                          return [];
                        }
                        ;
                        if (v12 instanceof PathAbsolute) {
                          return v12.value0;
                        }
                        ;
                        if (v12 instanceof PathRelative) {
                          return v12.value0;
                        }
                        ;
                        throw new Error("Failed pattern match at PureStack.Router (line 161, column 18 - line 164, column 36): " + [v12.constructor.name]);
                      }();
                      var v1 = parseRoute1({
                        path: path2,
                        verb: verb1
                      });
                      if (v1 instanceof Nothing) {
                        return runAPI1($$delete4($$Proxy.value)(handlers))(req);
                      }
                      ;
                      if (v1 instanceof Just) {
                        return v1.value0(get2($$Proxy.value)(handlers))(req);
                      }
                      ;
                      throw new Error("Failed pattern match at PureStack.Router (line 166, column 9 - line 168, column 63): " + [v1.constructor.name]);
                    }
                    ;
                    throw new Error("Failed pattern match at PureStack.Router (line 157, column 5 - line 168, column 63): " + [v.constructor.name]);
                  };
                }
              };
            };
          };
        };
      };
    };
  };
  var assert = function(dictApplicative) {
    var pure22 = pure(dictApplicative);
    return function(dictPlus) {
      var empty4 = empty2(dictPlus);
      return function(v) {
        if (v) {
          return pure22(unit);
        }
        ;
        if (!v) {
          return empty4;
        }
        ;
        throw new Error("Failed pattern match at PureStack.Router (line 137, column 1 - line 137, column 65): " + [v.constructor.name]);
      };
    };
  };
  var assert1 = /* @__PURE__ */ assert(applicativeMaybe)(plusMaybe);
  var parseRoute$div = function(dictParseRoute) {
    var parseRoute1 = parseRoute(dictParseRoute);
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return {
        parseRoute: function(v) {
          return bind2(uncons(v.path))(function(v1) {
            return discard2(assert1(v1.head === reflectSymbol2($$Proxy.value)))(function() {
              return parseRoute1({
                path: v1.tail,
                verb: v.verb
              });
            });
          });
        }
      };
    };
  };
  var parseRouteGETEffect = function(dictToResponse) {
    var toResponse1 = toResponse(dictToResponse);
    return {
      parseRoute: function(v) {
        return discard2(assert1(v.verb === "GET"))(function() {
          return discard2(assert1(eq12(v.path)([])))(function() {
            return pure12(function(handler) {
              return function(_req) {
                return function __do2() {
                  var resp = handler();
                  return toResponse1(resp)({
                    status: 200,
                    statusText: "OK",
                    headers: []
                  });
                };
              };
            });
          });
        });
      }
    };
  };
  var parseRoutePOSTUnitEffect = function(dictToResponse) {
    var toResponse1 = toResponse(dictToResponse);
    return {
      parseRoute: function(v) {
        return discard2(assert1(eq12(v.path)([])))(function() {
          return discard2(assert1(v.verb === "POST"))(function() {
            return pure12(function(handler) {
              return function(req) {
                return function __do2() {
                  var resp = handler();
                  return toResponse1(resp)({
                    status: 200,
                    statusText: "OK",
                    headers: []
                  });
                };
              };
            });
          });
        });
      }
    };
  };

  // output/Main/index.js
  var gEncodeJsonCons2 = /* @__PURE__ */ gEncodeJsonCons(encodeJsonInt);
  var aIsSymbol = {
    reflectSymbol: function() {
      return "a";
    }
  };
  var fooIsSymbol = {
    reflectSymbol: function() {
      return "foo";
    }
  };
  var barIsSymbol = {
    reflectSymbol: function() {
      return "bar";
    }
  };
  var run4 = /* @__PURE__ */ run3()(/* @__PURE__ */ runAPICons(/* @__PURE__ */ parseRoute$div(/* @__PURE__ */ parseRoute$div(/* @__PURE__ */ parseRoutePOSTUnitEffect(/* @__PURE__ */ toResponseRecord(/* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons2(gEncodeJsonNil)(aIsSymbol)())())))({
    reflectSymbol: function() {
      return "qux";
    }
  }))(fooIsSymbol))()(/* @__PURE__ */ runAPICons(/* @__PURE__ */ parseRoute$div(/* @__PURE__ */ parseRoute$div(/* @__PURE__ */ parseRoute$div(/* @__PURE__ */ parseRouteGETEffect(/* @__PURE__ */ toResponseRecord(/* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons2(/* @__PURE__ */ gEncodeJsonCons(encodeJsonJNumber)(gEncodeJsonNil)({
    reflectSymbol: function() {
      return "b";
    }
  })())(aIsSymbol)())())))(fooIsSymbol))(barIsSymbol))(fooIsSymbol))()(runAPINilRow)(fooIsSymbol)()())(barIsSymbol)()());
  var pure3 = /* @__PURE__ */ pure(applicativeEffect);
  var main = function __do() {
    log2("\u{1F35D}")();
    return serve(run4({
      foo: pure3({
        a: 1,
        b: 1
      }),
      bar: pure3({
        a: 8
      })
    }))();
  };

  // <stdin>
  main();
})();
