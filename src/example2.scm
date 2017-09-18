(load stdlib)

(defr person (name age favLang))

(def Haskello (person-make "Haskello Javason" 1337 'scheme))

;(person? Haskello)
(person-age Haskello)