(ns leap)

(defn leap-year? [year]
  (cond
    (zero? (rem year 400)) true
    (zero? (rem year 100)) false
    (zero? (rem year 4)) true
    :else false))
