;;; package --- Summary
;;; Commentary:
;;; Code:

(ert-deftest stats-test-quartile-1-3-elts ()
  (should (=
           (pk/quartile '(1 2 3) 1)
           1)))

(ert-deftest stats-test-quartile-2-3-elts ()
  (should (=
           (pk/quartile '(1 2 3) 2)
           2)))

(ert-deftest stats-test-quartile-3-3-elts ()
  (should (=
           (pk/quartile '(1 2 3) 3)
           3)))

(ert-deftest stats-test-quartile-1-3-elts-include ()
  (should (=
           (pk/quartile '(1 2 3) 1 :include)
           1.5)))

(ert-deftest stats-test-quartile-2-3-elts-include ()
  (should (=
           (pk/quartile '(1 2 3) 2 :include)
           2)))

(ert-deftest stats-test-quartile-3-3-elts-include ()
  (should (=
           (pk/quartile '(1 2 3) 3 :include)
           2.5)))

(ert-deftest stats-test-quartile-2-2-elts ()
  (should (=
           (pk/quartile '(1 2) 2)
           1.5)))

(ert-deftest stats-test-quartile-2-2-elts-include ()
  (should (=
           (pk/quartile '(1 2) 2 :include)
           1.5)))

(ert-deftest stats-test-quartile-1-0-elts ()
  (should-not (pk/quartile '() 1)))

(ert-deftest stats-test-quartile-1-0-elts-include ()
  (should-not (pk/quartile '() 1 :include)))

(ert-deftest stats-test-quartile-2-0-elts ()
  (should-not (pk/quartile '() 2)))

(ert-deftest stats-test-quartile-2-0-elts-include ()
  (should-not (pk/quartile '() 2 :include)))

(ert-deftest stats-test-quartile-3-0-elts ()
  (should-not (pk/quartile '() 3)))

(ert-deftest stats-test-quartile-3-0-elts-include ()
  (should-not (pk/quartile '() 3 :include)))

(ert-deftest stats-test-quartile-1-1-elt ()
  (should-not (pk/quartile '(1) 1)))

(ert-deftest stats-test-quartile-1-1-elt-include ()
  (should-not (pk/quartile '(1) 1 :include)))

(ert-deftest stats-test-quartile-2-1-elt ()
  (should-not (pk/quartile '(1) 2)))

(ert-deftest stats-test-quartile-2-1-elt-include ()
  (should-not (pk/quartile '(1) 2 :include)))

(ert-deftest stats-test-quartile-3-1-elt ()
  (should-not (pk/quartile '(1) 3)))

(ert-deftest stats-test-quartile-3-1-elt-include ()
  (should-not (pk/quartile '(1) 3 :include)))

(ert-deftest stats-test-quartile-1-2-elts ()
  (should-not (pk/quartile '(1 2) 1)))

(ert-deftest stats-test-quartile-1-2-elts-include ()
  (should-not (pk/quartile '(1 2) 1 :include)))

(ert-deftest stats-test-quartile-3-2-elts ()
  (should-not (pk/quartile '(1 2) 3)))

(ert-deftest stats-test-quartile-3-2-elts-include ()
  (should-not (pk/quartile '(1 2) 3 :include)))

(ert-deftest stats-test-quartile-1-wiki-sample ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 43 47 49) 1)
           15)))

(ert-deftest stats-test-quartile-2-wiki-sample ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 43 47 49) 2)
           40)))

(ert-deftest stats-test-quartile-3-wiki-sample ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 43 47 49) 3)
           43)))

(ert-deftest stats-test-quartile-1-wiki-sample-include ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 43 47 49) 1 :include)
           25.5)))

(ert-deftest stats-test-quartile-2-wiki-sample-include ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 43 47 49) 2 :include)
           40)))

(ert-deftest stats-test-quartile-3-wiki-sample-include ()
  (should (=
           (pk/quartile '(6 7 15 36 39 40 41 42 43 47 49) 3 :include)
           42.5)))

(ert-deftest stats-test-median-wiki-sample ()
  (should (=
           (pk/median '(6 7 15 36 39 40 41 42 43 47 49))
           40)))

(ert-deftest stats-test-five-nums-wiki-sample ()
  (should (equal
           (pk/five-nums '(6 7 15 36 39 40 41 42 43 47 49))
           '(6 15 40 43 49))))

(ert-deftest stats-test-five-nums-wiki-sample-include ()
  (should (equal
           (pk/five-nums '(6 7 15 36 39 40 41 42 43 47 49) :include)
           '(6 25.5 40 42.5 49))))

(ert-deftest stats-tests-fice-nums-with-header ()
  (should (equal
           (pk/five-nums-with-header '(6 7 15 36 39 40 41 42 43 47 49))
           '(("min" "q1" "med" "q3" "max")
             hline
             (6 15 40 43 49)))))

(ert-deftest stats-tests-fice-nums-with-header-include ()
  (should (equal
           (pk/five-nums-with-header '(6 7 15 36 39 40 41 42 43 47 49) :include)
           '(("min" "q1" "med" "q3" "max")
             hline
             (6 25.5 40 42.5 49)))))

(provide 'stats.t)
;;; stats.t ends here
