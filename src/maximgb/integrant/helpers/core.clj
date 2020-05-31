(ns maximgb.integrant.helpers.core)

(def config-fn-keyword ::config-fn)

(defmacro config-fn [& fn-tail]
  "Creates a system configuration function, if sub-system/service recieves such a function as configuration value,
   then it will call that function with service configuration map as the only parameter and use the value returned
   as configuration option value."
  `(as-config-fn (fn ~@fn-tail)))


(defn as-config-fn [fn]
  "Marks a `fn` function as system configuration function, see `config-fn` macro for more information."
  (with-meta fn {config-fn-keyword true}))


(defn is-config-fn? [fn]
  "Checks if given `fn` should be threated as system configuration function."
  (-> fn
      (meta)
      (config-fn-keyword)
      (= true)))


(defn realize-value [v config]
  "Realizes system configuraiton value.
   Checks if the passed `v` value is a system configuration function and if it is then applies the function to `config`
   and returns the result otherwise returns `v` unmodified."
  (if (and (fn? v) (is-config-fn? v))
    (v config)
    v))


(defn realize-config [config]
  "Sames as `realize-value` but realizes entire sub-system/service configuration map."
  (let [ks (keys config)]
    (zipmap ks (map #(realize-value (get config %)
                                    config)
                    ks))))
