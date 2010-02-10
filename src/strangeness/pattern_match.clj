(ns strangeness.pattern-match
  "Macros for pattern matching clojure sequences."
  {:author "Alexander Konstantinov"})

(declare emit-match)

(defn- emit-symbol-match [struct-name pattern make-then else syms]
  (cond
    (= '_ pattern) ;; ignore wildcard
      (make-then syms)
    (contains? syms pattern) ;; already bound => emit equality check
      `(if-not (= ~struct-name ~pattern)
         ~else
         ~(make-then syms))
    :else ;; new name => emit binding
      `(let [~pattern ~struct-name]
         ~(make-then (conj syms pattern)))))

(defn- split-match [struct-name orig-pattern alt-pattern make-then else syms]
  (let [alt-name (symbol (str \a struct-name))]
    (emit-match struct-name ;; emit code to match against the orignal
                orig-pattern
                (fn [new-syms]
                  `(let [~alt-name ~struct-name]
                     ;; later match against an alternative pattern
                     ~(emit-match alt-name
                                  alt-pattern
                                  make-then
                                  else
                                  new-syms)))
                else
                syms)))

(defn- emit-vector-match [struct-name pattern make-then else syms]
  (cond
    (empty? pattern) ;; empty sequences match agains []
      `(if (seq ~struct-name)
         ~else
         ~(make-then syms))
    ;; slurpy ('&) pattern
    (and (= (count pattern) 2) (= '& (first pattern)))
      `(if-not (sequential? ~struct-name)
         ~else
         ~(emit-match struct-name
                      (second pattern)
                      make-then
                      else
                      syms))
    ;; :as form
    (and (> (count pattern) 2) (= :as (first (take-last 2 pattern))))
      (let [alt-pattern (last pattern)
            orig-pattern (subvec pattern 0 (- (count pattern) 2))]
        (split-match struct-name orig-pattern alt-pattern make-then else syms))
    :else ;; split into head and tail
      (let [head-pattern (first pattern)
            tail-pattern (vec (next pattern))
            head-name (symbol (str \f struct-name))
            tail-name (symbol (str \n struct-name))]
        `(if-not (sequential? ~struct-name)
           ~else
           (let [~head-name (first ~struct-name)
                 ~tail-name (next ~struct-name)]
             ~(emit-match head-name
                          head-pattern ;; for the head of the pattern
                          (fn new-make-then
                            [new-syms] ;; pass newly discovered symbols
                            (emit-match tail-name ;; for the tail
                                        tail-pattern
                                        make-then
                                        else
                                        new-syms))
                          else
                          syms)))))) ;; register the new name

(defn- sanitize-map-pattern [pattern]
  (when-not (every? symbol?
                    (concat (:keys pattern) (:syms pattern) (:strs pattern)))
    (throw (IllegalArgumentException. "Directives must consist of symbols")))
  (-> pattern
      (conj (zipmap (:keys pattern)
                    (map keyword (:keys pattern))))
      (conj (zipmap (:syms pattern)
                    (:syms pattern)))
      (conj (zipmap (:strs pattern)
                    (map str (:strs pattern))))
      (dissoc :keys :syms :strs)))

(defn- emit-map-match [struct-name pattern make-then else syms]
  (if (:as pattern)
    (let [alt-pattern (:as pattern)
          orig-pattern (dissoc pattern :as)]
      (split-match struct-name orig-pattern alt-pattern make-then else syms))
    (let [pattern (sanitize-map-pattern pattern)
          failkw (keyword (gensym "fail"))
          val-names (repeatedly (partial gensym "val"))
          bindings (vec (mapcat (fn [name key]
                                  (list name
                                        (if (or (keyword? key) (symbol? key))
                                          `(~key ~struct-name ~failkw)
                                          `(get ~struct-name ~key ~failkw))))
                                val-names (vals pattern)))
          make-then (reduce (fn [next-then [key-pattern substruct-name]]
                              (fn [new-syms]
                                `(if (identical? ~failkw ~substruct-name)
                                   ~else
                                   ~(emit-match substruct-name
                                                key-pattern
                                                next-then
                                                else
                                                new-syms))))
                            make-then
                            (reverse (map vector (keys pattern) val-names)))]
      `(if-not (instance? clojure.lang.ILookup ~struct-name)
         ~else
         (let ~bindings
           ~(make-then syms))))))

(defn- emit-match [struct-name pattern make-then else syms]
  (cond
    (symbol? pattern)
      (emit-symbol-match struct-name pattern make-then else syms)
    (vector? pattern)
      (emit-vector-match struct-name pattern make-then else syms)
    (map? pattern)
      (emit-map-match struct-name pattern make-then else syms)
    ;; allows to use '~ to match against locals or vector literals
    (and (seq? pattern) (= `unquote (first pattern)))
      `(if-not (= ~struct-name ~(second pattern))
         ~else
         ~(make-then syms))
    :else ;; a literal pattern (evaluated)
      `(if-not (= ~struct-name ~pattern)
         ~else
         ~(make-then syms))))

(defmacro if-match
  "Matches expr against pattern, and evaluates action with new locals
  introduced by the pattern if the match is successful. Evaluates else
  otherwise.

  Patterns use the destructuring bind syntax from let and fn (eg. vectors
  to match sequences and maps to match associative structures) and support
  the same directives (& :as :keys :syms :strs).
  
  The pattern can contain _ as wildcards that match anything, and is allowed
  to contain an identifier multiple times, which only matches if the structures
  are equal (using =).
  The pattern may contain arbitrary expressions, either as literals or as
  unquoted (with ~) expressions, which the corresponding substructure must
  be equal to.

  Example:
  (if-match '(x (y y) ([1 2] 3 4 5) y y) [a ['y 'y :as b] [~[1 2] c & _] & b]
    [a b c]
    nil)
  -> [x (y y) 3]

  Warning: Do not use big else-clauses as they will get expanded many times
  and may cause bloat."
  ([expr pattern action] `(if-match ~expr ~pattern ~action nil))
  ([expr pattern action else]
     (let [expr-name (gensym)]
       `(let [~expr-name ~expr]
          ~(emit-match expr-name pattern (constantly action) else #{})))))

(defmacro match
  "clause => pattern action
  or
  clause => pattern :when guard action

  Matches expr against several patterns in succession. If a pattern matches
  (and the guard passes) evaluates to the corresponding action. If no pattern
  matches evaluates to nil.

  See if-match for pattern syntax"
  {:arglists '([expr & clauses])}
  ([expr] nil)
  ([expr pattern action & more]
     (let [failkw (keyword (gensym "fail"))
           expr-name (gensym "expr")
           guard? (= action :when)
           _ (when (and guard? (not (next more)))
               (throw (IllegalArgumentException. "clause missing")))
           guard (when guard? (first more))
           action (if guard? (second more) action)
           more (if guard? (nnext more) more)]
       `(let [~expr-name ~expr
              result# ~(if guard?
                         `(if-match ~expr-name ~pattern
                                    (if ~guard
                                      ~action
                                      ~failkw)
                                    ~failkw)
                         `(if-match ~expr-name ~pattern ~action ~failkw))]
          (if (identical? ~failkw result#)
            (match ~expr-name ~@more)
            result#)))))
