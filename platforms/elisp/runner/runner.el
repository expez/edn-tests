(require 'ert)
;; (require 'el-edn)
(require 'cl)

(defun test-name (file-path)
  (intern (file-name-nondirectory (file-name-sans-extension file-path))))

(defun empty-buffer? ()
  (s-blank?
   (s-trim (buffer-substring-no-properties (point-min)
                                           (point-max)))))

(defun slurp-file (file-path)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun create-happy-test (edn-path expected-path)
  "Generate tests from examples"
  (let ((expected (eval (read (slurp-file expected-path))))
        (edn (slurp-file edn-path)))
    `(ert-deftest ,(test-name edn-path) ()
       (should (equal ,expected (edn-parse ,edn))))))

(defmacro create-tests ()
  "Read in all the example files and create matching tests"
  (let* ((valid-edn-dir "../../../valid-edn")
         (result-dir "..")
         (file-names (mapcar (lambda (f)
                               (file-name-nondirectory (file-name-sans-extension f)))
                             (directory-files valid-edn-dir :absolute-paths "\.edn$")))
         tests)
    (dolist (file-name file-names)
      (push (create-happy-test (concat valid-edn-dir "/" file-name ".edn")
                               (concat result-dir "/" file-name ".el"))
            tests))
    `(progn ,@tests)))

(ert-deftest whitespace-triple-space ()
  (should (equal nil (edn-parse "
"))))

(ert-deftest whitespace-single-space ()
  (should (equal nil (edn-parse " "))))

(ert-deftest whitespace-comma ()
  (should (equal nil (edn-parse ","))))

(ert-deftest whitespace-blank ()
  (should (equal nil (edn-parse ""))))

(ert-deftest vector ()
  (should
   (equal
    '[1 2 3]
    (edn-parse "[1 2 3]"))))

(ert-deftest true ()
  (should (equal t (edn-parse "true"))))

(ert-deftest tag-unhandled ()
  (let ((unhandled (makehash 'equal))
        (val (makehash 'equal)))
    (puthash :first "Fred" val)
    (puthash :last "Mertz" val) ()
    (puthash :tag 'myapp/Person unhandled)
    (puthash :value 'myapp/Person val)
    (should
     (equal unhandled
            (edn-parse "#myapp/Person {:first \"Fred\" :last \"Mertz\"}"))))) ()

(ert-deftest tag-inst ()
  (should
   (equal '(7357 47698) (edn-parse "#inst \"1985-04-12T23:20:50.52Z\""))))

(ert-deftest symbol-with-slash ()
  (should
   (equal 'foo/bar (edn-parse "foo/bar"))))

(ert-deftest symbol-with-hash ()
  (should
   (equal 'some\#sort\#of\#symbol (edn-parse "some#sort#of#symbol"))))

(ert-deftest symbol-with-dash ()
  (should
   (equal 'foo-bar (edn-parse "foo-bar"))))

(ert-deftest symbol-vector ()
  (should
   (equal '[/ \. * ! _ \? $ % & = - +] (edn-parse "[/ . * ! _ ? $ % & = - +]"))))

(ert-deftest symbol-truefalse ()
  (should
   (equal 'truefalse (edn-parse "truefalse"))))

(ert-deftest symbol-trailing-dot ()
  (should
   (equal 'true. (edn-parse "true."))))

(ert-deftest symbol-slash ()
  (should
   (equal '/ (edn-parse "/"))))

(ert-deftest symbol-preceding-dot ()
  (should (equal '.true (edn-parse ".true"))))

(ert-deftest symbol-extra-colons ()
  (should (equal 'some:sort:of:symbol (edn-parse "some:sort:of:symbol"))))

(ert-deftest string ()
  (should
   (equal "this is a string" (edn-parse "\"this is a string\""))))

(ert-deftest string-with-quote ()
  (should (equal "this has an escaped \"quote in it"
                 (edn-parse "\"this has an escaped \\\"quote in it\""))))

(ert-deftest string-with-escaped-tab ()
  (should
   (equal "foo\\tbar" (edn-parse "\"foo\\tbar\""))))

(ert-deftest string-with-escaped-newline ()
  (should
   (equal "foo\\nbar" (edn-parse "\"foo\\nbar\""))))

(ert-deftest string-with-escaped-backslash ()
  (should
   (equal "this is a string \\ that has an escaped backslash"
          (edn-parse "\"this is a string \\\\ that has an escaped backslash\""))))

(ert-deftest string-with-bracket ()
  (should (equal "[" (edn-parse "\"[\""))))

(ert-deftest set ()
  (should
   (equal (list :set :of :distinct :izm) (edn-parse "#{:set :of :distinct :izm}"))))

(ert-deftest set-with-map ()
  (let ((m (make-hash-table :test 'equal)))
    (puthash :foo 'bar m)
    (should
     (equal (list m)
            (edn-parse "#{{:foo bar}}")))))

(ert-deftest set-with-list ()
  (should
   (equal '((foo bar)) (edn-parse "#{(foo bar)}"))))

(ert-deftest positive-symbol ()
  (should
   (equal '+some-symbol (edn-parse "+some-symbol"))))

(ert-deftest numbers ()
  (should
   (equal
    (vector 0 0 9923 -9923 9923 432N 12.32 -12.32 9923.23 223\.230M 45\.4E+43M 45\.4e+43M 4.5e+044)
    (edn-parse "[0 -0 9923 -9923 +9923 432N 12.32 -12.32 +9923.23 223.230M 45.4E+43M 45.4e+43M 45e+43]"))))

(ert-deftest test-nil ()
  (should (equal nil (edn-parse "nil"))))

(ert-deftest nil-keyed-map ()
  (let ((m (make-hash-table :test 'equal)))
    (puthash nil (vector :vector :of nil nil) m)
    (should
     (equal m (edn-parse "{nil [:vector :of nil nil]}")))))

(ert-deftest nested-list ()
  (should
   (equal '(a (b 42 (c d))) (edn-parse "(a (b 42 (c d)))"))))

(ert-deftest negative-symbol ()
  (should (equal '-symbol (edn-parse "-symbol"))))

(ert-deftest mixed-list ()
  (should (equal
           '(defproject com\.thortech/data\.edn "0.1.0-SNAPSHOT")
           (edn-parse "(defproject com.thortech/data.edn \"0.1.0-SNAPSHOT\")"))))

(ert-deftest map ()
  (let ((m (make-hash-table :test 'equal)))
    (puthash :this 'is m)
    (puthash 'a 'basic m)
    (puthash 'map 'tofu m)
    (should (equal m (edn-parse "{:this is a basic map tofu}")))))

(ert-deftest map-with-vector-key ()
  (let ((m (make-hash-table :test 'equal)))
    (puthash (vector 1 2 3) "some numbers" m)
    (should (equal m (edn-parse "{[1 2 3] \"some numbers\"}")))))

(ert-deftest keyword ()
  (should (equal :namespace\.of\.some\.length/keyword-name
                 (edn-parse ":namespace.of.some.length/keyword-name"))))

(ert-deftest hash-slash-hash-keyword ()
  (should (equal :\#/\# (edn-parse ":#/#"))))

(ert-deftest hash-slash-colon-char-keyword ()
  (should (equal :\#/:a (edn-parse ":#/:a"))))

(ert-deftest hash-keyword ()
  (should (equal :\#foo (edn-parse ":#foo"))))

(ert-deftest false ()
  (should (equal nil (edn-parse "false "))))

(ert-deftest empty-list ()
  (should (equal nil (edn-parse "()"))))

(ert-deftest discard-with-comment ()
  (should (equal (vector 'a 'd) (edn-parse "[a #_ ;we are discarding what comes next
 c d]"))))

(ert-deftest discard-touching-item ()
  (should (equal (vector 'a 'b 'd) (edn-parse "[a b #_c d]"))))

(ert-deftest discard-outside-form ()
  (should (equal nil (edn-parse "#_ a"))))

(ert-deftest discard-in-vector ()
  (should (equal (vector 'a 'b 'd) (edn-parse "[a b #_ c d]"))))

(ert-deftest discard-entire-form ()
  (should (equal (vector 'a 'b 'c 'd) (edn-parse "[a b #_ [a b] c d]"))))

(ert-deftest decimal-symbol ()
  (should (equal '\.another-symbol (edn-parse ".another-symbol"))))

(ert-deftest comment ()
  (should (equal (vector 'valid 'vector 'more 'vector 'items)
                 (edn-parse "[valid vector
 ;;comment in vector
 more vector items]"))))

(ert-deftest comment-trailing ()
  (should
   (equal '[valid more items] (edn-parse "[valid;touching trailing comment
 more items]"))))

(ert-deftest commas-no-one-cares ()
  (should
   (equal (vector 'a 'b 'c 'd) (edn-parse "[a ,,,,,, b,,,,,c ,d]"))))

(ert-deftest character-vector ()
  (should
   (equal (vector 'c 'newline 'return 'space 'tab)
          (edn-parse "[\\c \\newline \\return \\space \\tab]"))))

(ert-deftest basic-list ()
  (should (equal (list 'a 'b 42) (edn-parse "(a b 42)"))))
