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

// output/Data.Boolean/index.js
var otherwise = true;

// output/Data.Function/index.js
var flip = function(f) {
  return function(b) {
    return function(a) {
      return f(a)(b);
    };
  };
};

// output/Data.Unit/foreign.js
var unit = void 0;

// output/Data.Functor/index.js
var map = function(dict) {
  return dict.map;
};

// output/Control.Applicative/index.js
var pure = function(dict) {
  return dict.pure;
};

// output/Control.Bind/index.js
var discard = function(dict) {
  return dict.discard;
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

// output/Data.Semigroup/foreign.js
var concatString = function(s1) {
  return function(s2) {
    return s1 + s2;
  };
};

// output/Data.Semigroup/index.js
var semigroupString = {
  append: concatString
};
var append = function(dict) {
  return dict.append;
};

// output/Control.Alt/index.js
var alt = function(dict) {
  return dict.alt;
};

// output/Data.Bounded/foreign.js
var topChar = String.fromCharCode(65535);
var bottomChar = String.fromCharCode(0);
var topNumber = Number.POSITIVE_INFINITY;
var bottomNumber = Number.NEGATIVE_INFINITY;

// output/Data.Ord/foreign.js
var unsafeCompareImpl = function(lt) {
  return function(eq2) {
    return function(gt) {
      return function(x) {
        return function(y) {
          return x < y ? lt : x === y ? eq2 : gt;
        };
      };
    };
  };
};
var ordIntImpl = unsafeCompareImpl;
var ordCharImpl = unsafeCompareImpl;

// output/Data.Eq/foreign.js
var refEq = function(r1) {
  return function(r2) {
    return r1 === r2;
  };
};
var eqIntImpl = refEq;
var eqCharImpl = refEq;

// output/Data.Eq/index.js
var eqInt = {
  eq: eqIntImpl
};
var eqChar = {
  eq: eqCharImpl
};

// output/Data.Ordering/index.js
var LT = /* @__PURE__ */ function() {
  function LT2() {
  }
  ;
  LT2.value = new LT2();
  return LT2;
}();
var GT = /* @__PURE__ */ function() {
  function GT2() {
  }
  ;
  GT2.value = new GT2();
  return GT2;
}();
var EQ = /* @__PURE__ */ function() {
  function EQ2() {
  }
  ;
  EQ2.value = new EQ2();
  return EQ2;
}();

// output/Data.Ring/foreign.js
var intSub = function(x) {
  return function(y) {
    return x - y | 0;
  };
};

// output/Data.Semiring/foreign.js
var intAdd = function(x) {
  return function(y) {
    return x + y | 0;
  };
};
var intMul = function(x) {
  return function(y) {
    return x * y | 0;
  };
};

// output/Data.Semiring/index.js
var semiringInt = {
  add: intAdd,
  zero: 0,
  mul: intMul,
  one: 1
};

// output/Data.Ring/index.js
var ringInt = {
  sub: intSub,
  Semiring0: function() {
    return semiringInt;
  }
};

// output/Data.Ord/index.js
var ordInt = /* @__PURE__ */ function() {
  return {
    compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqInt;
    }
  };
}();
var ordChar = /* @__PURE__ */ function() {
  return {
    compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqChar;
    }
  };
}();
var compare = function(dict) {
  return dict.compare;
};
var lessThanOrEq = function(dictOrd) {
  var compare3 = compare(dictOrd);
  return function(a1) {
    return function(a2) {
      var v = compare3(a1)(a2);
      if (v instanceof GT) {
        return false;
      }
      ;
      return true;
    };
  };
};

// output/Data.Bounded/index.js
var top = function(dict) {
  return dict.top;
};
var boundedChar = {
  top: topChar,
  bottom: bottomChar,
  Ord0: function() {
    return ordChar;
  }
};
var bottom = function(dict) {
  return dict.bottom;
};

// output/Data.Show/foreign.js
var showIntImpl = function(n) {
  return n.toString();
};
var showCharImpl = function(c) {
  var code = c.charCodeAt(0);
  if (code < 32 || code === 127) {
    switch (c) {
      case "\x07":
        return "'\\a'";
      case "\b":
        return "'\\b'";
      case "\f":
        return "'\\f'";
      case "\n":
        return "'\\n'";
      case "\r":
        return "'\\r'";
      case "	":
        return "'\\t'";
      case "\v":
        return "'\\v'";
    }
    return "'\\" + code.toString(10) + "'";
  }
  return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
};
var showStringImpl = function(s) {
  var l = s.length;
  return '"' + s.replace(
    /[\0-\x1F\x7F"\\]/g,
    // eslint-disable-line no-control-regex
    function(c, i) {
      switch (c) {
        case '"':
        case "\\":
          return "\\" + c;
        case "\x07":
          return "\\a";
        case "\b":
          return "\\b";
        case "\f":
          return "\\f";
        case "\n":
          return "\\n";
        case "\r":
          return "\\r";
        case "	":
          return "\\t";
        case "\v":
          return "\\v";
      }
      var k = i + 1;
      var empty2 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
      return "\\" + c.charCodeAt(0).toString(10) + empty2;
    }
  ) + '"';
};

// output/Data.Show/index.js
var showString = {
  show: showStringImpl
};
var showInt = {
  show: showIntImpl
};
var showChar = {
  show: showCharImpl
};
var show = function(dict) {
  return dict.show;
};

// output/Data.Maybe/index.js
var identity2 = /* @__PURE__ */ identity(categoryFn);
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
var fromMaybe = function(a) {
  return maybe(a)(identity2);
};
var fromJust = function() {
  return function(v) {
    if (v instanceof Just) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
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
var tt = function(dict) {
  return dict.tt;
};
var not = function(dict) {
  return dict.not;
};
var implies = function(dict) {
  return dict.implies;
};
var ff = function(dict) {
  return dict.ff;
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
var conj = function(dict) {
  return dict.conj;
};
var heytingAlgebraFunction = function(dictHeytingAlgebra) {
  var ff1 = ff(dictHeytingAlgebra);
  var tt1 = tt(dictHeytingAlgebra);
  var implies1 = implies(dictHeytingAlgebra);
  var conj1 = conj(dictHeytingAlgebra);
  var disj1 = disj(dictHeytingAlgebra);
  var not1 = not(dictHeytingAlgebra);
  return {
    ff: function(v) {
      return ff1;
    },
    tt: function(v) {
      return tt1;
    },
    implies: function(f) {
      return function(g) {
        return function(a) {
          return implies1(f(a))(g(a));
        };
      };
    },
    conj: function(f) {
      return function(g) {
        return function(a) {
          return conj1(f(a))(g(a));
        };
      };
    },
    disj: function(f) {
      return function(g) {
        return function(a) {
          return disj1(f(a))(g(a));
        };
      };
    },
    not: function(f) {
      return function(a) {
        return not1(f(a));
      };
    }
  };
};

// output/Data.EuclideanRing/foreign.js
var intDegree = function(x) {
  return Math.min(Math.abs(x), 2147483647);
};
var intDiv = function(x) {
  return function(y) {
    if (y === 0)
      return 0;
    return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
  };
};
var intMod = function(x) {
  return function(y) {
    if (y === 0)
      return 0;
    var yy = Math.abs(y);
    return (x % yy + yy) % yy;
  };
};

// output/Data.CommutativeRing/index.js
var commutativeRingInt = {
  Ring0: function() {
    return ringInt;
  }
};

// output/Data.EuclideanRing/index.js
var mod = function(dict) {
  return dict.mod;
};
var euclideanRingInt = {
  degree: intDegree,
  div: intDiv,
  mod: intMod,
  CommutativeRing0: function() {
    return commutativeRingInt;
  }
};
var div = function(dict) {
  return dict.div;
};

// output/Data.Monoid/index.js
var monoidString = {
  mempty: "",
  Semigroup0: function() {
    return semigroupString;
  }
};
var mempty = function(dict) {
  return dict.mempty;
};

// output/Data.Tuple/index.js
var Tuple = /* @__PURE__ */ function() {
  function Tuple2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Tuple2.create = function(value0) {
    return function(value1) {
      return new Tuple2(value0, value1);
    };
  };
  return Tuple2;
}();
var fst = function(v) {
  return v.value0;
};

// output/Data.Bifunctor/index.js
var identity3 = /* @__PURE__ */ identity(categoryFn);
var bimap = function(dict) {
  return dict.bimap;
};
var lmap = function(dictBifunctor) {
  var bimap1 = bimap(dictBifunctor);
  return function(f) {
    return bimap1(f)(identity3);
  };
};
var bifunctorEither = {
  bimap: function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return new Left(v(v2.value0));
        }
        ;
        if (v2 instanceof Right) {
          return new Right(v1(v2.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Bifunctor (line 32, column 1 - line 34, column 36): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  }
};

// output/Data.Enum/foreign.js
function toCharCode(c) {
  return c.charCodeAt(0);
}
function fromCharCode(c) {
  return String.fromCharCode(c);
}

// output/Data.Traversable/foreign.js
var traverseArrayImpl = function() {
  function array1(a) {
    return [a];
  }
  function array2(a) {
    return function(b) {
      return [a, b];
    };
  }
  function array3(a) {
    return function(b) {
      return function(c) {
        return [a, b, c];
      };
    };
  }
  function concat2(xs) {
    return function(ys) {
      return xs.concat(ys);
    };
  }
  return function(apply2) {
    return function(map3) {
      return function(pure3) {
        return function(f) {
          return function(array) {
            function go(bot, top2) {
              switch (top2 - bot) {
                case 0:
                  return pure3([]);
                case 1:
                  return map3(array1)(f(array[bot]));
                case 2:
                  return apply2(map3(array2)(f(array[bot])))(f(array[bot + 1]));
                case 3:
                  return apply2(apply2(map3(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                default:
                  var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                  return apply2(map3(concat2)(go(bot, pivot)))(go(pivot, top2));
              }
            }
            return go(0, array.length);
          };
        };
      };
    };
  };
}();

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
var unwrap = function() {
  return coerce2;
};

// output/Data.Foldable/index.js
var foldr = function(dict) {
  return dict.foldr;
};
var foldl = function(dict) {
  return dict.foldl;
};
var intercalate = function(dictFoldable) {
  var foldl2 = foldl(dictFoldable);
  return function(dictMonoid) {
    var append2 = append(dictMonoid.Semigroup0());
    var mempty2 = mempty(dictMonoid);
    return function(sep) {
      return function(xs) {
        var go = function(v) {
          return function(v1) {
            if (v.init) {
              return {
                init: false,
                acc: v1
              };
            }
            ;
            return {
              init: false,
              acc: append2(v.acc)(append2(sep)(v1))
            };
          };
        };
        return foldl2(go)({
          init: true,
          acc: mempty2
        })(xs).acc;
      };
    };
  };
};
var foldMapDefaultR = function(dictFoldable) {
  var foldr2 = foldr(dictFoldable);
  return function(dictMonoid) {
    var append2 = append(dictMonoid.Semigroup0());
    var mempty2 = mempty(dictMonoid);
    return function(f) {
      return foldr2(function(x) {
        return function(acc) {
          return append2(f(x))(acc);
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

// output/Data.Identity/index.js
var Identity = function(x) {
  return x;
};
var functorIdentity = {
  map: function(f) {
    return function(m) {
      return f(m);
    };
  }
};
var applyIdentity = {
  apply: function(v) {
    return function(v1) {
      return v(v1);
    };
  },
  Functor0: function() {
    return functorIdentity;
  }
};
var bindIdentity = {
  bind: function(v) {
    return function(f) {
      return f(v);
    };
  },
  Apply0: function() {
    return applyIdentity;
  }
};
var applicativeIdentity = {
  pure: Identity,
  Apply0: function() {
    return applyIdentity;
  }
};
var monadIdentity = {
  Applicative0: function() {
    return applicativeIdentity;
  },
  Bind1: function() {
    return bindIdentity;
  }
};

// output/Data.Enum/index.js
var bottom1 = /* @__PURE__ */ bottom(boundedChar);
var top1 = /* @__PURE__ */ top(boundedChar);
var toEnum = function(dict) {
  return dict.toEnum;
};
var fromEnum = function(dict) {
  return dict.fromEnum;
};
var toEnumWithDefaults = function(dictBoundedEnum) {
  var toEnum1 = toEnum(dictBoundedEnum);
  var fromEnum1 = fromEnum(dictBoundedEnum);
  var bottom2 = bottom(dictBoundedEnum.Bounded0());
  return function(low) {
    return function(high) {
      return function(x) {
        var v = toEnum1(x);
        if (v instanceof Just) {
          return v.value0;
        }
        ;
        if (v instanceof Nothing) {
          var $140 = x < fromEnum1(bottom2);
          if ($140) {
            return low;
          }
          ;
          return high;
        }
        ;
        throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
      };
    };
  };
};
var defaultSucc = function(toEnum$prime) {
  return function(fromEnum$prime) {
    return function(a) {
      return toEnum$prime(fromEnum$prime(a) + 1 | 0);
    };
  };
};
var defaultPred = function(toEnum$prime) {
  return function(fromEnum$prime) {
    return function(a) {
      return toEnum$prime(fromEnum$prime(a) - 1 | 0);
    };
  };
};
var charToEnum = function(v) {
  if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
    return new Just(fromCharCode(v));
  }
  ;
  return Nothing.value;
};
var enumChar = {
  succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
  pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
  Ord0: function() {
    return ordChar;
  }
};
var boundedEnumChar = /* @__PURE__ */ function() {
  return {
    cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
    toEnum: charToEnum,
    fromEnum: toCharCode,
    Bounded0: function() {
      return boundedChar;
    },
    Enum1: function() {
      return enumChar;
    }
  };
}();

// output/Data.Char/index.js
var toCharCode2 = /* @__PURE__ */ fromEnum(boundedEnumChar);

// output/Data.Array/foreign.js
var replicateFill = function(count) {
  return function(value) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value);
  };
};
var replicatePolyfill = function(count) {
  return function(value) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count; i++) {
      result[n++] = value;
    }
    return result;
  };
};
var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
var fromFoldableImpl = function() {
  function Cons3(head4, tail2) {
    this.head = head4;
    this.tail = tail2;
  }
  var emptyList = {};
  function curryCons(head4) {
    return function(tail2) {
      return new Cons3(head4, tail2);
    };
  }
  function listToArray(list) {
    var result = [];
    var count = 0;
    var xs = list;
    while (xs !== emptyList) {
      result[count++] = xs.head;
      xs = xs.tail;
    }
    return result;
  }
  return function(foldr2) {
    return function(xs) {
      return listToArray(foldr2(curryCons)(emptyList)(xs));
    };
  };
}();
var sortByImpl = function() {
  function mergeFromTo(compare3, fromOrdering, xs1, xs2, from2, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
    var c;
    mid = from2 + (to - from2 >> 1);
    if (mid - from2 > 1)
      mergeFromTo(compare3, fromOrdering, xs2, xs1, from2, mid);
    if (to - mid > 1)
      mergeFromTo(compare3, fromOrdering, xs2, xs1, mid, to);
    i = from2;
    j = mid;
    k = from2;
    while (i < mid && j < to) {
      x = xs2[i];
      y = xs2[j];
      c = fromOrdering(compare3(x)(y));
      if (c > 0) {
        xs1[k++] = y;
        ++j;
      } else {
        xs1[k++] = x;
        ++i;
      }
    }
    while (i < mid) {
      xs1[k++] = xs2[i++];
    }
    while (j < to) {
      xs1[k++] = xs2[j++];
    }
  }
  return function(compare3) {
    return function(fromOrdering) {
      return function(xs) {
        var out;
        if (xs.length < 2)
          return xs;
        out = xs.slice(0);
        mergeFromTo(compare3, fromOrdering, out, xs.slice(0), 0, xs.length);
        return out;
      };
    };
  };
}();

// output/Control.Monad.Rec.Class/index.js
var Loop = /* @__PURE__ */ function() {
  function Loop2(value0) {
    this.value0 = value0;
  }
  ;
  Loop2.create = function(value0) {
    return new Loop2(value0);
  };
  return Loop2;
}();
var Done = /* @__PURE__ */ function() {
  function Done2(value0) {
    this.value0 = value0;
  }
  ;
  Done2.create = function(value0) {
    return new Done2(value0);
  };
  return Done2;
}();
var tailRecM = function(dict) {
  return dict.tailRecM;
};
var tailRec = function(f) {
  var go = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v instanceof Loop) {
        $copy_v = f(v.value0);
        return;
      }
      ;
      if (v instanceof Done) {
        $tco_done = true;
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 103, column 3 - line 103, column 25): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  return function($85) {
    return go(f($85));
  };
};
var monadRecIdentity = {
  tailRecM: function(f) {
    var runIdentity = function(v) {
      return v;
    };
    var $86 = tailRec(function($88) {
      return runIdentity(f($88));
    });
    return function($87) {
      return Identity($86($87));
    };
  },
  Monad0: function() {
    return monadIdentity;
  }
};

// output/Data.Array.ST/foreign.js
var sortByImpl2 = function() {
  function mergeFromTo(compare3, fromOrdering, xs1, xs2, from2, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
    var c;
    mid = from2 + (to - from2 >> 1);
    if (mid - from2 > 1)
      mergeFromTo(compare3, fromOrdering, xs2, xs1, from2, mid);
    if (to - mid > 1)
      mergeFromTo(compare3, fromOrdering, xs2, xs1, mid, to);
    i = from2;
    j = mid;
    k = from2;
    while (i < mid && j < to) {
      x = xs2[i];
      y = xs2[j];
      c = fromOrdering(compare3(x)(y));
      if (c > 0) {
        xs1[k++] = y;
        ++j;
      } else {
        xs1[k++] = x;
        ++i;
      }
    }
    while (i < mid) {
      xs1[k++] = xs2[i++];
    }
    while (j < to) {
      xs1[k++] = xs2[j++];
    }
  }
  return function(compare3) {
    return function(fromOrdering) {
      return function(xs) {
        return function() {
          if (xs.length < 2)
            return xs;
          mergeFromTo(compare3, fromOrdering, xs, xs.slice(0), 0, xs.length);
          return xs;
        };
      };
    };
  };
}();

// output/Data.Int/foreign.js
var fromStringAsImpl = function(just) {
  return function(nothing) {
    return function(radix) {
      var digits;
      if (radix < 11) {
        digits = "[0-" + (radix - 1).toString() + "]";
      } else if (radix === 11) {
        digits = "[0-9a]";
      } else {
        digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
      }
      var pattern = new RegExp("^[\\+\\-]?" + digits + "+$", "i");
      return function(s) {
        if (pattern.test(s)) {
          var i = parseInt(s, radix);
          return (i | 0) === i ? just(i) : nothing;
        } else {
          return nothing;
        }
      };
    };
  };
};

// output/Data.Int/index.js
var fromStringAs = /* @__PURE__ */ function() {
  return fromStringAsImpl(Just.create)(Nothing.value);
}();
var fromString = /* @__PURE__ */ fromStringAs(10);

// output/Data.String.CodePoints/foreign.js
var hasArrayFrom = typeof Array.from === "function";
var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
var hasCodePointAt = typeof String.prototype.codePointAt === "function";
var _unsafeCodePointAt0 = function(fallback) {
  return hasCodePointAt ? function(str) {
    return str.codePointAt(0);
  } : fallback;
};
var _codePointAt = function(fallback) {
  return function(Just2) {
    return function(Nothing2) {
      return function(unsafeCodePointAt02) {
        return function(index3) {
          return function(str) {
            var length6 = str.length;
            if (index3 < 0 || index3 >= length6)
              return Nothing2;
            if (hasStringIterator) {
              var iter = str[Symbol.iterator]();
              for (var i = index3; ; --i) {
                var o = iter.next();
                if (o.done)
                  return Nothing2;
                if (i === 0)
                  return Just2(unsafeCodePointAt02(o.value));
              }
            }
            return fallback(index3)(str);
          };
        };
      };
    };
  };
};
var _countPrefix = function(fallback) {
  return function(unsafeCodePointAt02) {
    if (hasStringIterator) {
      return function(pred) {
        return function(str) {
          var iter = str[Symbol.iterator]();
          for (var cpCount = 0; ; ++cpCount) {
            var o = iter.next();
            if (o.done)
              return cpCount;
            var cp = unsafeCodePointAt02(o.value);
            if (!pred(cp))
              return cpCount;
          }
        };
      };
    }
    return fallback;
  };
};
var _singleton = function(fallback) {
  return hasFromCodePoint ? String.fromCodePoint : fallback;
};
var _take = function(fallback) {
  return function(n) {
    if (hasStringIterator) {
      return function(str) {
        var accum = "";
        var iter = str[Symbol.iterator]();
        for (var i = 0; i < n; ++i) {
          var o = iter.next();
          if (o.done)
            return accum;
          accum += o.value;
        }
        return accum;
      };
    }
    return fallback(n);
  };
};

// output/Data.String.CodeUnits/foreign.js
var singleton2 = function(c) {
  return c;
};
var length3 = function(s) {
  return s.length;
};
var drop = function(n) {
  return function(s) {
    return s.substring(n);
  };
};

// output/Data.String.Unsafe/foreign.js
var charAt = function(i) {
  return function(s) {
    if (i >= 0 && i < s.length)
      return s.charAt(i);
    throw new Error("Data.String.Unsafe.charAt: Invalid index.");
  };
};

// output/Data.String.Common/foreign.js
var trim = function(s) {
  return s.trim();
};

// output/Data.String.Common/index.js
var $$null2 = function(s) {
  return s === "";
};

// output/Data.String.CodePoints/index.js
var $runtime_lazy = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
var div2 = /* @__PURE__ */ div(euclideanRingInt);
var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
var compare2 = /* @__PURE__ */ compare(ordInt);
var unsurrogate = function(lead) {
  return function(trail) {
    return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
  };
};
var isTrail = function(cu) {
  return 56320 <= cu && cu <= 57343;
};
var isLead = function(cu) {
  return 55296 <= cu && cu <= 56319;
};
var uncons = function(s) {
  var v = length3(s);
  if (v === 0) {
    return Nothing.value;
  }
  ;
  if (v === 1) {
    return new Just({
      head: fromEnum2(charAt(0)(s)),
      tail: ""
    });
  }
  ;
  var cu1 = fromEnum2(charAt(1)(s));
  var cu0 = fromEnum2(charAt(0)(s));
  var $43 = isLead(cu0) && isTrail(cu1);
  if ($43) {
    return new Just({
      head: unsurrogate(cu0)(cu1),
      tail: drop(2)(s)
    });
  }
  ;
  return new Just({
    head: cu0,
    tail: drop(1)(s)
  });
};
var unsafeCodePointAt0Fallback = function(s) {
  var cu0 = fromEnum2(charAt(0)(s));
  var $47 = isLead(cu0) && length3(s) > 1;
  if ($47) {
    var cu1 = fromEnum2(charAt(1)(s));
    var $48 = isTrail(cu1);
    if ($48) {
      return unsurrogate(cu0)(cu1);
    }
    ;
    return cu0;
  }
  ;
  return cu0;
};
var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
var fromCharCode2 = /* @__PURE__ */ function() {
  var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
  return function($76) {
    return singleton2($75($76));
  };
}();
var singletonFallback = function(v) {
  if (v <= 65535) {
    return fromCharCode2(v);
  }
  ;
  var lead = div2(v - 65536 | 0)(1024) + 55296 | 0;
  var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
  return fromCharCode2(lead) + fromCharCode2(trail);
};
var singleton3 = /* @__PURE__ */ _singleton(singletonFallback);
var takeFallback = function(v) {
  return function(v1) {
    if (v < 1) {
      return "";
    }
    ;
    var v2 = uncons(v1);
    if (v2 instanceof Just) {
      return singleton3(v2.value0.head) + takeFallback(v - 1 | 0)(v2.value0.tail);
    }
    ;
    return v1;
  };
};
var take2 = /* @__PURE__ */ _take(takeFallback);
var eqCodePoint = {
  eq: function(x) {
    return function(y) {
      return x === y;
    };
  }
};
var ordCodePoint = {
  compare: function(x) {
    return function(y) {
      return compare2(x)(y);
    };
  },
  Eq0: function() {
    return eqCodePoint;
  }
};
var countTail = function($copy_p) {
  return function($copy_s) {
    return function($copy_accum) {
      var $tco_var_p = $copy_p;
      var $tco_var_s = $copy_s;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(p, s, accum) {
        var v = uncons(s);
        if (v instanceof Just) {
          var $61 = p(v.value0.head);
          if ($61) {
            $tco_var_p = p;
            $tco_var_s = v.value0.tail;
            $copy_accum = accum + 1 | 0;
            return;
          }
          ;
          $tco_done = true;
          return accum;
        }
        ;
        $tco_done = true;
        return accum;
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_p, $tco_var_s, $copy_accum);
      }
      ;
      return $tco_result;
    };
  };
};
var countFallback = function(p) {
  return function(s) {
    return countTail(p)(s)(0);
  };
};
var countPrefix2 = /* @__PURE__ */ _countPrefix(countFallback)(unsafeCodePointAt0);
var takeWhile = function(p) {
  return function(s) {
    return take2(countPrefix2(p)(s))(s);
  };
};
var codePointAtFallback = function($copy_n) {
  return function($copy_s) {
    var $tco_var_n = $copy_n;
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(n, s) {
      var v = uncons(s);
      if (v instanceof Just) {
        var $66 = n === 0;
        if ($66) {
          $tco_done = true;
          return new Just(v.value0.head);
        }
        ;
        $tco_var_n = n - 1 | 0;
        $copy_s = v.value0.tail;
        return;
      }
      ;
      $tco_done = true;
      return Nothing.value;
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($tco_var_n, $copy_s);
    }
    ;
    return $tco_result;
  };
};
var codePointAt = function(v) {
  return function(v1) {
    if (v < 0) {
      return Nothing.value;
    }
    ;
    if (v === 0 && v1 === "") {
      return Nothing.value;
    }
    ;
    if (v === 0) {
      return new Just(unsafeCodePointAt0(v1));
    }
    ;
    return _codePointAt(codePointAtFallback)(Just.create)(Nothing.value)(unsafeCodePointAt0)(v)(v1);
  };
};
var boundedCodePoint = {
  bottom: 0,
  top: 1114111,
  Ord0: function() {
    return ordCodePoint;
  }
};
var boundedEnumCodePoint = /* @__PURE__ */ function() {
  return {
    cardinality: 1114111 + 1 | 0,
    fromEnum: function(v) {
      return v;
    },
    toEnum: function(n) {
      if (n >= 0 && n <= 1114111) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.String.CodePoints (line 63, column 1 - line 68, column 26): " + [n.constructor.name]);
    },
    Bounded0: function() {
      return boundedCodePoint;
    },
    Enum1: function() {
      return $lazy_enumCodePoint(0);
    }
  };
}();
var $lazy_enumCodePoint = /* @__PURE__ */ $runtime_lazy("enumCodePoint", "Data.String.CodePoints", function() {
  return {
    succ: defaultSucc(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
    pred: defaultPred(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
    Ord0: function() {
      return ordCodePoint;
    }
  };
});

// output/Data.CodePoint.Unicode/index.js
var fromEnum3 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
var isDecDigit = function(c) {
  var diff = fromEnum3(c) - toCharCode2("0") | 0;
  return diff <= 9 && diff >= 0;
};

// output/Control.Monad.Error.Class/index.js
var throwError = function(dict) {
  return dict.throwError;
};

// output/Parsing/index.js
var unwrap2 = /* @__PURE__ */ unwrap();
var ParseState = /* @__PURE__ */ function() {
  function ParseState2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  ParseState2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new ParseState2(value0, value1, value2);
      };
    };
  };
  return ParseState2;
}();
var ParseError = /* @__PURE__ */ function() {
  function ParseError2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  ParseError2.create = function(value0) {
    return function(value1) {
      return new ParseError2(value0, value1);
    };
  };
  return ParseError2;
}();
var More = /* @__PURE__ */ function() {
  function More2(value0) {
    this.value0 = value0;
  }
  ;
  More2.create = function(value0) {
    return new More2(value0);
  };
  return More2;
}();
var Lift = /* @__PURE__ */ function() {
  function Lift2(value0) {
    this.value0 = value0;
  }
  ;
  Lift2.create = function(value0) {
    return new Lift2(value0);
  };
  return Lift2;
}();
var Stop = /* @__PURE__ */ function() {
  function Stop2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Stop2.create = function(value0) {
    return function(value1) {
      return new Stop2(value0, value1);
    };
  };
  return Stop2;
}();
var functorParserT = {
  map: function(f) {
    return function(v) {
      return function(state1, more, lift1, $$throw, done) {
        return more(function(v1) {
          return v(state1, more, lift1, $$throw, function(state2, a) {
            return more(function(v2) {
              return done(state2, f(a));
            });
          });
        });
      };
    };
  }
};
var applyParserT = {
  apply: function(v) {
    return function(v1) {
      return function(state1, more, lift1, $$throw, done) {
        return more(function(v2) {
          return v(state1, more, lift1, $$throw, function(state2, f) {
            return more(function(v3) {
              return v1(state2, more, lift1, $$throw, function(state3, a) {
                return more(function(v4) {
                  return done(state3, f(a));
                });
              });
            });
          });
        });
      };
    };
  },
  Functor0: function() {
    return functorParserT;
  }
};
var bindParserT = {
  bind: function(v) {
    return function(next) {
      return function(state1, more, lift1, $$throw, done) {
        return more(function(v1) {
          return v(state1, more, lift1, $$throw, function(state2, a) {
            return more(function(v2) {
              var v3 = next(a);
              return v3(state2, more, lift1, $$throw, done);
            });
          });
        });
      };
    };
  },
  Apply0: function() {
    return applyParserT;
  }
};
var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindParserT);
var applicativeParserT = {
  pure: function(a) {
    return function(state1, v, v1, v2, done) {
      return done(state1, a);
    };
  },
  Apply0: function() {
    return applyParserT;
  }
};
var monadParserT = {
  Applicative0: function() {
    return applicativeParserT;
  },
  Bind1: function() {
    return bindParserT;
  }
};
var monadThrowParseErrorParse = {
  throwError: function(err) {
    return function(state1, v, v1, $$throw, v2) {
      return $$throw(state1, err);
    };
  },
  Monad0: function() {
    return monadParserT;
  }
};
var throwError2 = /* @__PURE__ */ throwError(monadThrowParseErrorParse);
var altParserT = {
  alt: function(v) {
    return function(v1) {
      return function(v2, more, lift1, $$throw, done) {
        return more(function(v3) {
          return v(new ParseState(v2.value0, v2.value1, false), more, lift1, function(v4, err) {
            return more(function(v5) {
              if (v4.value2) {
                return $$throw(v4, err);
              }
              ;
              return v1(v2, more, lift1, $$throw, done);
            });
          }, done);
        });
      };
    };
  },
  Functor0: function() {
    return functorParserT;
  }
};
var stateParserT = function(k) {
  return function(state1, v, v1, v2, done) {
    var v3 = k(state1);
    return done(v3.value1, v3.value0);
  };
};
var runParserT$prime = function(dictMonadRec) {
  var Monad0 = dictMonadRec.Monad0();
  var map3 = map(Monad0.Bind1().Apply0().Functor0());
  var pure1 = pure(Monad0.Applicative0());
  var tailRecM3 = tailRecM(dictMonadRec);
  return function(state1) {
    return function(v) {
      var go = function($copy_step) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(step2) {
          var v1 = step2(unit);
          if (v1 instanceof More) {
            $copy_step = v1.value0;
            return;
          }
          ;
          if (v1 instanceof Lift) {
            $tco_done = true;
            return map3(Loop.create)(v1.value0);
          }
          ;
          if (v1 instanceof Stop) {
            $tco_done = true;
            return pure1(new Done(new Tuple(v1.value1, v1.value0)));
          }
          ;
          throw new Error("Failed pattern match at Parsing (line 152, column 13 - line 158, column 32): " + [v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_step);
        }
        ;
        return $tco_result;
      };
      return tailRecM3(go)(function(v1) {
        return v(state1, More.create, Lift.create, function(state2, err) {
          return new Stop(state2, new Left(err));
        }, function(state2, res) {
          return new Stop(state2, new Right(res));
        });
      });
    };
  };
};
var position = /* @__PURE__ */ stateParserT(function(v) {
  return new Tuple(v.value1, v);
});
var parseErrorPosition = function(v) {
  return v.value1;
};
var parseErrorMessage = function(v) {
  return v.value0;
};
var initialPos = {
  index: 0,
  line: 1,
  column: 1
};
var runParserT = function(dictMonadRec) {
  var map3 = map(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
  var runParserT$prime1 = runParserT$prime(dictMonadRec);
  return function(s) {
    return function(p) {
      var initialState = new ParseState(s, initialPos, false);
      return map3(fst)(runParserT$prime1(initialState)(p));
    };
  };
};
var runParserT1 = /* @__PURE__ */ runParserT(monadRecIdentity);
var runParser = function(s) {
  var $281 = runParserT1(s);
  return function($282) {
    return unwrap2($281($282));
  };
};
var failWithPosition = function(message2) {
  return function(pos) {
    return throwError2(new ParseError(message2, pos));
  };
};
var fail = function(message2) {
  return bindFlipped2(failWithPosition(message2))(position);
};

// output/Parsing.Combinators/index.js
var alt2 = /* @__PURE__ */ alt(altParserT);
var withErrorMessage = function(p) {
  return function(msg) {
    return alt2(p)(fail("Expected " + msg));
  };
};

// output/Data.Array.NonEmpty.Internal/foreign.js
var traverse1Impl = function() {
  function Cont(fn) {
    this.fn = fn;
  }
  var emptyList = {};
  var ConsCell = function(head4, tail2) {
    this.head = head4;
    this.tail = tail2;
  };
  function finalCell(head4) {
    return new ConsCell(head4, emptyList);
  }
  function consList(x) {
    return function(xs) {
      return new ConsCell(x, xs);
    };
  }
  function listToArray(list) {
    var arr = [];
    var xs = list;
    while (xs !== emptyList) {
      arr.push(xs.head);
      xs = xs.tail;
    }
    return arr;
  }
  return function(apply2) {
    return function(map3) {
      return function(f) {
        var buildFrom = function(x, ys) {
          return apply2(map3(consList)(f(x)))(ys);
        };
        var go = function(acc, currentLen, xs) {
          if (currentLen === 0) {
            return acc;
          } else {
            var last3 = xs[currentLen - 1];
            return new Cont(function() {
              var built = go(buildFrom(last3, acc), currentLen - 1, xs);
              return built;
            });
          }
        };
        return function(array) {
          var acc = map3(finalCell)(f(array[array.length - 1]));
          var result = go(acc, array.length - 1, array);
          while (result instanceof Cont) {
            result = result.fn();
          }
          return map3(listToArray)(result);
        };
      };
    };
  };
}();

// output/Data.Function.Uncurried/foreign.js
var mkFn5 = function(fn) {
  return function(a, b, c, d, e) {
    return fn(a)(b)(c)(d)(e);
  };
};

// output/Parsing.String/index.js
var fromEnum4 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
var mod3 = /* @__PURE__ */ mod(euclideanRingInt);
var fromJust2 = /* @__PURE__ */ fromJust();
var toEnum2 = /* @__PURE__ */ toEnum(boundedEnumChar);
var show2 = /* @__PURE__ */ show(showChar);
var updatePosSingle = function(v) {
  return function(cp) {
    return function(after) {
      var v1 = fromEnum4(cp);
      if (v1 === 10) {
        return {
          index: v.index + 1 | 0,
          line: v.line + 1 | 0,
          column: 1
        };
      }
      ;
      if (v1 === 13) {
        var v2 = codePointAt(0)(after);
        if (v2 instanceof Just && fromEnum4(v2.value0) === 10) {
          return {
            index: v.index + 1 | 0,
            line: v.line,
            column: v.column
          };
        }
        ;
        return {
          index: v.index + 1 | 0,
          line: v.line + 1 | 0,
          column: 1
        };
      }
      ;
      if (v1 === 9) {
        return {
          index: v.index + 1 | 0,
          line: v.line,
          column: (v.column + 8 | 0) - mod3(v.column - 1 | 0)(8) | 0
        };
      }
      ;
      return {
        index: v.index + 1 | 0,
        line: v.line,
        column: v.column + 1 | 0
      };
    };
  };
};
var updatePosString = function($copy_pos) {
  return function($copy_before) {
    return function($copy_after) {
      var $tco_var_pos = $copy_pos;
      var $tco_var_before = $copy_before;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(pos, before, after) {
        var v = uncons(before);
        if (v instanceof Nothing) {
          $tco_done = true;
          return pos;
        }
        ;
        if (v instanceof Just) {
          var newPos = function() {
            if ($$null2(v.value0.tail)) {
              return updatePosSingle(pos)(v.value0.head)(after);
            }
            ;
            if (otherwise) {
              return updatePosSingle(pos)(v.value0.head)(v.value0.tail);
            }
            ;
            throw new Error("Failed pattern match at Parsing.String (line 165, column 7 - line 167, column 52): " + []);
          }();
          $tco_var_pos = newPos;
          $tco_var_before = v.value0.tail;
          $copy_after = after;
          return;
        }
        ;
        throw new Error("Failed pattern match at Parsing.String (line 161, column 36 - line 168, column 38): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_pos, $tco_var_before, $copy_after);
      }
      ;
      return $tco_result;
    };
  };
};
var satisfy = function(f) {
  return mkFn5(function(v) {
    return function(v1) {
      return function(v2) {
        return function($$throw) {
          return function(done) {
            var v3 = uncons(v.value0);
            if (v3 instanceof Nothing) {
              return $$throw(v, new ParseError("Unexpected EOF", v.value1));
            }
            ;
            if (v3 instanceof Just) {
              var cp = fromEnum4(v3.value0.head);
              var $85 = cp < 0 || cp > 65535;
              if ($85) {
                return $$throw(v, new ParseError("Expected Char", v.value1));
              }
              ;
              var ch = fromJust2(toEnum2(cp));
              var $86 = f(ch);
              if ($86) {
                return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), ch);
              }
              ;
              return $$throw(v, new ParseError("Predicate unsatisfied", v.value1));
            }
            ;
            throw new Error("Failed pattern match at Parsing.String (line 114, column 7 - line 129, column 75): " + [v3.constructor.name]);
          };
        };
      };
    };
  });
};
var eof = /* @__PURE__ */ mkFn5(function(v) {
  return function(v1) {
    return function(v2) {
      return function($$throw) {
        return function(done) {
          var $133 = $$null2(v.value0);
          if ($133) {
            return done(new ParseState(v.value0, v.value1, true), unit);
          }
          ;
          return $$throw(v, new ParseError("Expected EOF", v.value1));
        };
      };
    };
  };
});
var consumeWith = function(f) {
  return mkFn5(function(v) {
    return function(v1) {
      return function(v2) {
        return function($$throw) {
          return function(done) {
            var v3 = f(v.value0);
            if (v3 instanceof Left) {
              return $$throw(v, new ParseError(v3.value0, v.value1));
            }
            ;
            if (v3 instanceof Right) {
              return done(new ParseState(v3.value0.remainder, updatePosString(v.value1)(v3.value0.consumed)(v3.value0.remainder), !$$null2(v3.value0.consumed)), v3.value0.value);
            }
            ;
            throw new Error("Failed pattern match at Parsing.String (line 286, column 7 - line 290, column 121): " + [v3.constructor.name]);
          };
        };
      };
    };
  });
};
var $$char = function(c) {
  return withErrorMessage(satisfy(function(v) {
    return v === c;
  }))(show2(c));
};

// output/Parsing.String.Basic/index.js
var takeWhile1 = function(predicate) {
  return consumeWith(function(s) {
    var value = takeWhile(predicate)(s);
    var len = length3(value);
    var $27 = len > 0;
    if ($27) {
      return new Right({
        consumed: value,
        remainder: drop(length3(value))(s),
        value
      });
    }
    ;
    return new Left("Expected character satisfying predicate");
  });
};

// output/Main/index.js
var show3 = /* @__PURE__ */ show(showString);
var show1 = /* @__PURE__ */ show(showInt);
var intercalate4 = /* @__PURE__ */ intercalate(foldableArray)(monoidString);
var map2 = /* @__PURE__ */ map(functorMaybe);
var lessThanOrEq2 = /* @__PURE__ */ lessThanOrEq(ordInt);
var bind2 = /* @__PURE__ */ bind(bindParserT);
var discard2 = /* @__PURE__ */ discard(discardUnit)(bindParserT);
var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean));
var pure2 = /* @__PURE__ */ pure(applicativeParserT);
var toString = function(v) {
  return v;
};
var showIPv4 = {
  show: function(v) {
    return "(IPv4 " + (show3(v) + ")");
  }
};
var prettyError = function(err) {
  var msg = parseErrorMessage(err);
  var v = parseErrorPosition(err);
  return msg + (" starting at position " + show1(v.column));
};
var pOctet = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ takeWhile1(isDecDigit))("a decimal digit");
var mkIPv4 = function(octet1) {
  return function(octet2) {
    return function(octet3) {
      return function(octet4) {
        return intercalate4(".")([octet1, octet2, octet3, octet4]);
      };
    };
  };
};
var isOctetValid = function(octet) {
  return fromMaybe(false)(map2(flip(lessThanOrEq2)(255))(fromString(octet)));
};
var parser = /* @__PURE__ */ bind2(pOctet)(function(octet1) {
  return bind2($$char("."))(function() {
    return bind2(pOctet)(function(octet2) {
      return bind2($$char("."))(function() {
        return bind2(pOctet)(function(octet3) {
          return bind2($$char("."))(function() {
            return bind2(pOctet)(function(octet4) {
              return discard2(withErrorMessage(eof)("end of string"))(function() {
                var $27 = not2(isOctetValid)(octet1);
                if ($27) {
                  return failWithPosition("Octet can only be 0-255")({
                    column: 1,
                    index: 0,
                    line: 1
                  });
                }
                ;
                var $28 = not2(isOctetValid)(octet2);
                if ($28) {
                  return failWithPosition("Octet can only be 0-255")({
                    column: 5,
                    index: 4,
                    line: 1
                  });
                }
                ;
                var $29 = not2(isOctetValid)(octet3);
                if ($29) {
                  return failWithPosition("Octet can only be 0-255")({
                    column: 9,
                    index: 8,
                    line: 1
                  });
                }
                ;
                var $30 = not2(isOctetValid)(octet4);
                if ($30) {
                  return failWithPosition("Octet can only be 0-255")({
                    column: 13,
                    index: 12,
                    line: 1
                  });
                }
                ;
                return pure2(mkIPv4(octet1)(octet2)(octet3)(octet4));
              });
            });
          });
        });
      });
    });
  });
});
var parse_ = /* @__PURE__ */ function() {
  var $33 = lmap(bifunctorEither)(prettyError);
  var $34 = flip(runParser)(parser);
  return function($35) {
    return $33($34(trim($35)));
  };
}();
var eqIPv4 = {
  eq: function(v) {
    return function(v1) {
      return v === v1;
    };
  }
};
export {
  eqIPv4,
  parse_,
  showIPv4,
  toString
};
