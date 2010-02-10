(ns strangeness.pattern-match-test
  (:use strangeness.pattern-match
        clojure.test))

(deftest equality
  (testing "Matching on equality"
    (are [obj pattern] (if-match obj pattern true false)
      "hello" "hello"
      :kwd :kwd
      nil nil
      '() '()
      'sym 'sym
      "constructed string" ~(str 'constructed \space "string"))
    
    (are [obj pattern] (not (if-match obj pattern true false))
      :this :that
      1 2
      8 ~[]
      true false)))

(deftest sequences
  (testing "Matching on sequences"
    (testing "(length)"
      (are [seq pattern] (if-match seq pattern true false)
        nil []
        [1 2 3] [_ _ _]
        (range 2) [_ _]
        (iterate inc 0) [_ & _])

      (are [seq pattern] (not (if-match seq pattern true false))
        [1 2] [_]
        [1] [_ _ _]
        nil [_]))

    (testing "(deep)"
      (are [seq pattern] (if-match seq pattern true false)
        '(1 2 (3 4) (((((5)))))) [1 2 [3 4] [[[[[5]]]]]]
        '(nil (nil) (nil nil)) [[] [[]] [[] []]]))))

(deftype Foo [a b])

(deftest associatives
  (testing "Matching on associative structures"
    (testing "(on maps)"
      (are [map pattern] (if-match map pattern true false)
        {1 1} {_ 1}
        {\a 1 \b 5} {_ \a _ \b}
        {} {})

      (are [map pattern] (not (if-match map pattern true false))
        {} {_ :a}
        {1 2} {_ 2}))

    (testing "(on vectors)"
      (are [vec pattern] (if-match vec pattern true false)
        [0 1 2] {0 0, 1 1, 2 2}
        ["a" :vector] {"a" 0, :vector 1}))

    (testing "(on deftypes)"
      (are [type pattern] (if-match type pattern true false)
        (Foo 1 2) {1 :a 2 :b}
        (Foo 1 2 {} {:c 3}) {3 :c}))))

(deftest binding-names
  (testing "Destructuring sequences"
    (is (= [1 2 3] (if-match [1 2 3] [a b c] [a b c])))
    (is (= [1 2 3] (if-match (range 1 4) it it)))
    (is (= [1 [2 3]] (if-match '(1 2 3) [h & t] [h t])))
    (is (= [2 [6]]
           (if-match [1 [2 [3] 4] 5 6] [_ [a [_] 4] 5 & rest]
                     [a rest]))))

  (testing "Destructuring maps"
    (is (= 30 (if-match {:name "Joe" :age 30} {age :age} age)))
    (is (= nil (if-match {:key nil} {it :key} it)))
    (is (= [:it 6] (if-match {nil :it, false 6} {a nil, b false} [a b]))))

  (testing "Destructuring sequences and maps")
  (is (= [1 2 [3 4]]
         (if-match '(1 {"hi" [3 4]} 2 3 4) [a {[3 4 :as rest] "hi"} b & rest]
           [a b rest]))))

(deftest match-macro
  (testing "Basic matching"
    (is (= "two"
           (match 2
             0 "zero"
             1 "one"
             2 "two"
             _ "bigger")))

    (is (= nil
           (match nil
             nil nil
             _ (throw (Exception. "Shouldn't reach this")))))

    (is (= 6
           (match 6
             [a b c] "Should fail"
             a a)))

    (is (= nil
           (match 5
             [_ _] "Not sequential"
             {_ :a} "Not associative"))))

  (testing "Guarded matching"
    (is (= :keyword
           (match :keyword
             a :when (number? a) :number
             a :when (keyword? a) :keyword)))

    (is (= \c
           (match [\a \b \c]
             [a b c :as whole] :when (seq? whole) (+ a b c)
             {third 2} third)))))
