["^ ","~:resource-id",["~:shadow.build.classpath/resource","com/cognitect/transit/eq.js"],"~:js","goog.provide(\"com.cognitect.transit.eq\");\ngoog.require(\"com.cognitect.transit.util\");\ngoog.scope(function() {\n  var eq = com.cognitect.transit.eq, util = com.cognitect.transit.util;\n  eq.hashCodeProperty = \"transit$hashCode$\";\n  eq.hashCodeCounter = 1;\n  eq.equals = function(x, y) {\n    if (x == null) {\n      return y == null;\n    } else {\n      if (x === y) {\n        return true;\n      } else {\n        if (typeof x === \"object\") {\n          if (util.isArray(x)) {\n            if (util.isArray(y)) {\n              if (x.length === y.length) {\n                for (var i = 0; i < x.length; i++) {\n                  if (!eq.equals(x[i], y[i])) {\n                    return false;\n                  }\n                }\n                return true;\n              } else {\n                return false;\n              }\n            } else {\n              return false;\n            }\n          } else {\n            if (x.com$cognitect$transit$equals) {\n              return x.com$cognitect$transit$equals(y);\n            } else {\n              if (y != null && typeof y === \"object\") {\n                if (y.com$cognitect$transit$equals) {\n                  return y.com$cognitect$transit$equals(x);\n                } else {\n                  var xklen = 0, yklen = util.objectKeys(y).length;\n                  for (var p in x) {\n                    if (!x.hasOwnProperty(p)) {\n                      continue;\n                    }\n                    xklen++;\n                    if (!y.hasOwnProperty(p)) {\n                      return false;\n                    } else {\n                      if (!eq.equals(x[p], y[p])) {\n                        return false;\n                      }\n                    }\n                  }\n                  return xklen === yklen;\n                }\n              } else {\n                return false;\n              }\n            }\n          }\n        } else {\n          return false;\n        }\n      }\n    }\n  };\n  eq.hashCombine = function(seed, hash) {\n    return seed ^ hash + 2654435769 + (seed << 6) + (seed >> 2);\n  };\n  eq.stringCodeCache = {};\n  eq.stringCodeCacheSize = 0;\n  eq.STR_CACHE_MAX = 256;\n  eq.hashString = function(str) {\n    var cached = eq.stringCodeCache[str];\n    if (cached != null) {\n      return cached;\n    }\n    var code = 0;\n    for (var i = 0; i < str.length; ++i) {\n      code = 31 * code + str.charCodeAt(i);\n      code %= 4294967296;\n    }\n    eq.stringCodeCacheSize++;\n    if (eq.stringCodeCacheSize >= eq.STR_CACHE_MAX) {\n      eq.stringCodeCache = {};\n      eq.stringCodeCacheSize = 1;\n    }\n    eq.stringCodeCache[str] = code;\n    return code;\n  };\n  eq.hashMapLike = function(m) {\n    var code = 0;\n    if (m.forEach != null) {\n      m.forEach(function(val, key, m) {\n        code = (code + (eq.hashCode(key) ^ eq.hashCode(val))) % 4503599627370496;\n      });\n    } else {\n      var keys = util.objectKeys(m);\n      for (var i = 0; i < keys.length; i++) {\n        var key = keys[i];\n        var val = m[key];\n        code = (code + (eq.hashCode(key) ^ eq.hashCode(val))) % 4503599627370496;\n      }\n    }\n    return code;\n  };\n  eq.hashArrayLike = function(arr) {\n    var code = 0;\n    if (util.isArray(arr)) {\n      for (var i = 0; i < arr.length; i++) {\n        code = eq.hashCombine(code, eq.hashCode(arr[i]));\n      }\n    } else {\n      if (arr.forEach) {\n        arr.forEach(function(x, i) {\n          code = eq.hashCombine(code, eq.hashCode(x));\n        });\n      }\n    }\n    return code;\n  };\n  eq.hashCode = function(x) {\n    if (x == null) {\n      return 0;\n    } else {\n      switch(typeof x) {\n        case \"number\":\n          return x;\n          break;\n        case \"boolean\":\n          return x === true ? 1 : 0;\n          break;\n        case \"string\":\n          return eq.hashString(x);\n          break;\n        case \"function\":\n          var code = x[eq.hashCodeProperty];\n          if (code) {\n            return code;\n          } else {\n            code = eq.hashCodeCounter;\n            if (typeof Object.defineProperty != \"undefined\") {\n              Object.defineProperty(x, eq.hashCodeProperty, {value:code, enumerable:false});\n            } else {\n              x[eq.hashCodeProperty] = code;\n            }\n            eq.hashCodeCounter++;\n            return code;\n          }\n          break;\n        default:\n          if (x instanceof Date) {\n            return x.valueOf();\n          } else {\n            if (util.isArray(x)) {\n              return eq.hashArrayLike(x);\n            }\n          }\n          if (x.com$cognitect$transit$hashCode) {\n            return x.com$cognitect$transit$hashCode();\n          } else {\n            return eq.hashMapLike(x);\n          }\n          break;\n      }\n    }\n  };\n  eq.extendToEQ = function(obj, opts) {\n    obj.com$cognitect$transit$hashCode = opts[\"hashCode\"];\n    obj.com$cognitect$transit$equals = opts[\"equals\"];\n    return obj;\n  };\n});\n","~:source","// Copyright 2014 Cognitect. All Rights Reserved.\n//\n// Licensed under the Apache License, Version 2.0 (the \"License\");\n// you may not use this file except in compliance with the License.\n// You may obtain a copy of the License at\n//\n//      http://www.apache.org/licenses/LICENSE-2.0\n//\n// Unless required by applicable law or agreed to in writing, software\n// distributed under the License is distributed on an \"AS-IS\" BASIS,\n// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n// See the License for the specific language governing permissions and\n// limitations under the License.\n\ngoog.provide(\"com.cognitect.transit.eq\");\ngoog.require(\"com.cognitect.transit.util\");\n\ngoog.scope(function() {\n\nvar eq   = com.cognitect.transit.eq,\n    util = com.cognitect.transit.util;\n\n/**\n * @const\n * @type {string}\n */\neq.hashCodeProperty = \"transit$hashCode$\";\n\n/**\n * @type {number}\n */\neq.hashCodeCounter = 1;\n\neq.equals = function (x, y) {\n    if(x == null) {\n        return y == null;\n    } else if(x === y) {\n        return true;\n    } else if(typeof x === \"object\") {\n        if(util.isArray(x)) {\n            if(util.isArray(y)) {\n                if(x.length === y.length) {\n                    for(var i = 0; i < x.length; i++) {\n                        if(!eq.equals(x[i], y[i])) {\n                            return false;\n                        }\n                    }\n                    return true;\n                } else {\n                    return false;\n                }\n            } else {\n                return false;\n            }\n        } else if(x.com$cognitect$transit$equals) {\n            return x.com$cognitect$transit$equals(y);\n        } else if((y != null) && (typeof y === \"object\")) {\n            if(y.com$cognitect$transit$equals) {\n                return y.com$cognitect$transit$equals(x);\n            } else {\n                var xklen = 0,\n                    yklen = util.objectKeys(y).length;\n                for(var p in x) {\n                    if(!x.hasOwnProperty(p)) continue;\n                    xklen++;\n                    if(!y.hasOwnProperty(p)) {\n                        return false;\n                    } else {\n                        if(!eq.equals(x[p], y[p])) {\n                            return false;\n                        }\n                    }\n                }\n                return xklen === yklen;\n            }\n        } else {\n            return false;\n        }\n    } else {\n        return false\n    }\n};\n\neq.hashCombine = function(seed, hash) {\n    return seed ^ (hash + 0x9e3779b9 + (seed << 6) + (seed >> 2));\n};\n\neq.stringCodeCache     = {};\neq.stringCodeCacheSize = 0;\n\n/**\n * @const\n * @type {number}\n */\neq.STR_CACHE_MAX       = 256;\n\neq.hashString = function(str) {\n    // a la goog.string.HashCode\n    // http://docs.closure-library.googlecode.com/git/local_closure_goog_string_string.js.source.html#line1206\n    var cached = eq.stringCodeCache[str];\n    if(cached != null) {\n        return cached;\n    }\n    var code = 0;\n    for (var i = 0; i < str.length; ++i) {\n        code = 31 * code + str.charCodeAt(i);\n        code %= 0x100000000;\n    }\n    eq.stringCodeCacheSize++;\n    if(eq.stringCodeCacheSize >= eq.STR_CACHE_MAX) {\n        eq.stringCodeCache = {};\n        eq.stringCodeCacheSize = 1;\n    }\n    eq.stringCodeCache[str] = code;\n    return code;\n};\n\neq.hashMapLike = function(m) {\n    var code = 0;\n    // ES6 Map-like case\n    if(m.forEach != null) {\n        m.forEach(function(val, key, m) {\n            code = (code + (eq.hashCode(key) ^ eq.hashCode(val))) % 4503599627370496;\n        });\n    } else {\n        // JS Object case\n        var keys = util.objectKeys(m);\n        for(var i = 0; i < keys.length; i++) {\n            var key = keys[i];\n            var val = m[key];\n            code = (code + (eq.hashCode(key) ^ eq.hashCode(val))) % 4503599627370496;\n        }\n    }\n    return code;\n};\n\neq.hashArrayLike = function(arr) {\n    var code = 0;\n    if(util.isArray(arr)) {\n        for(var i = 0; i < arr.length; i++) {\n            code = eq.hashCombine(code, eq.hashCode(arr[i]));\n        }\n    } else if(arr.forEach) {\n        arr.forEach(function(x, i) {\n            code = eq.hashCombine(code, eq.hashCode(x));\n        });\n    }\n    return code;\n};\n\neq.hashCode = function(x) {\n    if(x == null) {\n        return 0;\n    } else {\n        switch(typeof x) {\n        case 'number':\n            return x;\n            break;\n        case 'boolean':\n            return x === true ? 1 : 0;\n            break;\n        case 'string':\n            return eq.hashString(x);\n            break;\n        case 'function':\n            var code = x[eq.hashCodeProperty];\n            if(code) {\n                return code;\n            } else {\n                code = eq.hashCodeCounter;\n                if(typeof Object.defineProperty != \"undefined\") {\n                    Object.defineProperty(x, eq.hashCodeProperty, {\n                        value: code,\n                        enumerable: false\n                    });\n                } else {\n                    x[eq.hashCodeProperty] = code;\n                }\n                eq.hashCodeCounter++;    \n                return code;\n            }\n            break;\n        default:\n            if(x instanceof Date) {\n                return x.valueOf();\n            } else if(util.isArray(x)) {\n                return eq.hashArrayLike(x);\n            } if(x.com$cognitect$transit$hashCode) {\n                return x.com$cognitect$transit$hashCode();\n            } else {\n                return eq.hashMapLike(x);\n            }\n            break;\n        }\n    }\n}\n\neq.extendToEQ = function(obj, opts) {\n    obj.com$cognitect$transit$hashCode = opts[\"hashCode\"];\n    obj.com$cognitect$transit$equals = opts[\"equals\"];\n    return obj;\n}\n\n});\n","~:compiled-at",1627944017130,"~:source-map-json","{\n\"version\":3,\n\"file\":\"com.cognitect.transit.eq.js\",\n\"lineCount\":172,\n\"mappings\":\"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\",\n\"sources\":[\"com/cognitect/transit/eq.js\"],\n\"sourcesContent\":[\"// Copyright 2014 Cognitect. All Rights Reserved.\\n//\\n// Licensed under the Apache License, Version 2.0 (the \\\"License\\\");\\n// you may not use this file except in compliance with the License.\\n// You may obtain a copy of the License at\\n//\\n//      http://www.apache.org/licenses/LICENSE-2.0\\n//\\n// Unless required by applicable law or agreed to in writing, software\\n// distributed under the License is distributed on an \\\"AS-IS\\\" BASIS,\\n// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\\n// See the License for the specific language governing permissions and\\n// limitations under the License.\\n\\ngoog.provide(\\\"com.cognitect.transit.eq\\\");\\ngoog.require(\\\"com.cognitect.transit.util\\\");\\n\\ngoog.scope(function() {\\n\\nvar eq   = com.cognitect.transit.eq,\\n    util = com.cognitect.transit.util;\\n\\n/**\\n * @const\\n * @type {string}\\n */\\neq.hashCodeProperty = \\\"transit$hashCode$\\\";\\n\\n/**\\n * @type {number}\\n */\\neq.hashCodeCounter = 1;\\n\\neq.equals = function (x, y) {\\n    if(x == null) {\\n        return y == null;\\n    } else if(x === y) {\\n        return true;\\n    } else if(typeof x === \\\"object\\\") {\\n        if(util.isArray(x)) {\\n            if(util.isArray(y)) {\\n                if(x.length === y.length) {\\n                    for(var i = 0; i < x.length; i++) {\\n                        if(!eq.equals(x[i], y[i])) {\\n                            return false;\\n                        }\\n                    }\\n                    return true;\\n                } else {\\n                    return false;\\n                }\\n            } else {\\n                return false;\\n            }\\n        } else if(x.com$cognitect$transit$equals) {\\n            return x.com$cognitect$transit$equals(y);\\n        } else if((y != null) && (typeof y === \\\"object\\\")) {\\n            if(y.com$cognitect$transit$equals) {\\n                return y.com$cognitect$transit$equals(x);\\n            } else {\\n                var xklen = 0,\\n                    yklen = util.objectKeys(y).length;\\n                for(var p in x) {\\n                    if(!x.hasOwnProperty(p)) continue;\\n                    xklen++;\\n                    if(!y.hasOwnProperty(p)) {\\n                        return false;\\n                    } else {\\n                        if(!eq.equals(x[p], y[p])) {\\n                            return false;\\n                        }\\n                    }\\n                }\\n                return xklen === yklen;\\n            }\\n        } else {\\n            return false;\\n        }\\n    } else {\\n        return false\\n    }\\n};\\n\\neq.hashCombine = function(seed, hash) {\\n    return seed ^ (hash + 0x9e3779b9 + (seed << 6) + (seed >> 2));\\n};\\n\\neq.stringCodeCache     = {};\\neq.stringCodeCacheSize = 0;\\n\\n/**\\n * @const\\n * @type {number}\\n */\\neq.STR_CACHE_MAX       = 256;\\n\\neq.hashString = function(str) {\\n    // a la goog.string.HashCode\\n    // http://docs.closure-library.googlecode.com/git/local_closure_goog_string_string.js.source.html#line1206\\n    var cached = eq.stringCodeCache[str];\\n    if(cached != null) {\\n        return cached;\\n    }\\n    var code = 0;\\n    for (var i = 0; i < str.length; ++i) {\\n        code = 31 * code + str.charCodeAt(i);\\n        code %= 0x100000000;\\n    }\\n    eq.stringCodeCacheSize++;\\n    if(eq.stringCodeCacheSize >= eq.STR_CACHE_MAX) {\\n        eq.stringCodeCache = {};\\n        eq.stringCodeCacheSize = 1;\\n    }\\n    eq.stringCodeCache[str] = code;\\n    return code;\\n};\\n\\neq.hashMapLike = function(m) {\\n    var code = 0;\\n    // ES6 Map-like case\\n    if(m.forEach != null) {\\n        m.forEach(function(val, key, m) {\\n            code = (code + (eq.hashCode(key) ^ eq.hashCode(val))) % 4503599627370496;\\n        });\\n    } else {\\n        // JS Object case\\n        var keys = util.objectKeys(m);\\n        for(var i = 0; i < keys.length; i++) {\\n            var key = keys[i];\\n            var val = m[key];\\n            code = (code + (eq.hashCode(key) ^ eq.hashCode(val))) % 4503599627370496;\\n        }\\n    }\\n    return code;\\n};\\n\\neq.hashArrayLike = function(arr) {\\n    var code = 0;\\n    if(util.isArray(arr)) {\\n        for(var i = 0; i < arr.length; i++) {\\n            code = eq.hashCombine(code, eq.hashCode(arr[i]));\\n        }\\n    } else if(arr.forEach) {\\n        arr.forEach(function(x, i) {\\n            code = eq.hashCombine(code, eq.hashCode(x));\\n        });\\n    }\\n    return code;\\n};\\n\\neq.hashCode = function(x) {\\n    if(x == null) {\\n        return 0;\\n    } else {\\n        switch(typeof x) {\\n        case 'number':\\n            return x;\\n            break;\\n        case 'boolean':\\n            return x === true ? 1 : 0;\\n            break;\\n        case 'string':\\n            return eq.hashString(x);\\n            break;\\n        case 'function':\\n            var code = x[eq.hashCodeProperty];\\n            if(code) {\\n                return code;\\n            } else {\\n                code = eq.hashCodeCounter;\\n                if(typeof Object.defineProperty != \\\"undefined\\\") {\\n                    Object.defineProperty(x, eq.hashCodeProperty, {\\n                        value: code,\\n                        enumerable: false\\n                    });\\n                } else {\\n                    x[eq.hashCodeProperty] = code;\\n                }\\n                eq.hashCodeCounter++;    \\n                return code;\\n            }\\n            break;\\n        default:\\n            if(x instanceof Date) {\\n                return x.valueOf();\\n            } else if(util.isArray(x)) {\\n                return eq.hashArrayLike(x);\\n            } if(x.com$cognitect$transit$hashCode) {\\n                return x.com$cognitect$transit$hashCode();\\n            } else {\\n                return eq.hashMapLike(x);\\n            }\\n            break;\\n        }\\n    }\\n}\\n\\neq.extendToEQ = function(obj, opts) {\\n    obj.com$cognitect$transit$hashCode = opts[\\\"hashCode\\\"];\\n    obj.com$cognitect$transit$equals = opts[\\\"equals\\\"];\\n    return obj;\\n}\\n\\n});\\n\"],\n\"names\":[\"goog\",\"provide\",\"require\",\"scope\",\"eq\",\"com\",\"cognitect\",\"transit\",\"util\",\"hashCodeProperty\",\"hashCodeCounter\",\"equals\",\"eq.equals\",\"x\",\"y\",\"isArray\",\"length\",\"i\",\"com$cognitect$transit$equals\",\"xklen\",\"yklen\",\"objectKeys\",\"p\",\"hasOwnProperty\",\"hashCombine\",\"eq.hashCombine\",\"seed\",\"hash\",\"stringCodeCache\",\"stringCodeCacheSize\",\"STR_CACHE_MAX\",\"hashString\",\"eq.hashString\",\"str\",\"cached\",\"code\",\"charCodeAt\",\"hashMapLike\",\"eq.hashMapLike\",\"m\",\"forEach\",\"val\",\"key\",\"hashCode\",\"keys\",\"hashArrayLike\",\"eq.hashArrayLike\",\"arr\",\"eq.hashCode\",\"Object\",\"defineProperty\",\"value\",\"enumerable\",\"Date\",\"valueOf\",\"com$cognitect$transit$hashCode\",\"extendToEQ\",\"eq.extendToEQ\",\"obj\",\"opts\"]\n}\n"]