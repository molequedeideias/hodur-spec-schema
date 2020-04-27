(ns hodur-spec-schema.octopus
  (:require [clojure.spec.alpha :as s]
            [datascript.core :as d]
            [datascript.query-v3 :as q]
            [camel-snake-kebab.core :refer [->kebab-case-string]]
            [meander.epsilon :as m]
            #_[sc.api :as sc]))

(defn ^:private get-ids-by-node-type [conn node-type]
  (case node-type
    :type (d/q '[:find [?e ...]
                 :in $ ?node-type
                 :where
                 [?e :node/type ?node-type]
                 [?e :spec/tag true]
                 [?e :type/nature :user]]
               @conn node-type)
    (d/q '[:find [?e ...]
           :in $ ?node-type
           :where
           [?e :node/type ?node-type]
           [?e :spec/tag true]]
         @conn node-type)))

(defn ^:private prepend-core-ns [sym]
  (when (not (nil? sym))
    (if (namespace sym)
      sym
      (symbol "clojure.core" (str sym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare get-spec-form)

(defmulti ^:private get-spec-name
          (fn [obj opts]
            (cond
              (and (seqable? obj)
                   (every? #(= :param (:node/type %)) obj))
              :param-group

              :default
              (:node/type obj))))

(defn ^:private get-spec-entity-name
  [type-name
   {:keys [prefix-entities] :or {prefix-entities "modelo.entidade"}}]
  (keyword (name prefix-entities)
           (->kebab-case-string type-name)))

(defn ^:private get-spec-entity-enum-name
  [type-name
   {:keys [prefix-enums] :or {prefix-enums "enum"}}]
  (keyword (name prefix-enums)
           (->kebab-case-string type-name)))

(defn ^:private get-namespace-for-spec
  [prefix type-name]
  (if prefix
    (str (name prefix) "." (->kebab-case-string type-name))
    (->kebab-case-string type-name)))



(defn ^:private get-spec-field-name
  [type-name field-name
   {:keys [prefix]}]
  (keyword (get-namespace-for-spec prefix type-name)
           (->kebab-case-string field-name)))

(defn ^:private get-spec-param-name
  [type-name field-name param-name
   {:keys [prefix]}]
  (keyword (str (get-namespace-for-spec prefix type-name) "."
                (->kebab-case-string field-name))
           (->kebab-case-string param-name)))

(defn ^:private get-spec-param-group-name
  [type-name field-name
   {:keys [prefix params-postfix group-type] :or {params-postfix "%"} :as opts}]
  (keyword (get-namespace-for-spec prefix type-name)
           (str (->kebab-case-string field-name)
                (case group-type
                  :map ""
                  :tuple "-ordered")
                params-postfix)))

(defmethod get-spec-name :type
  [{:keys [type/kebab-case-name
           type/enum]} opts]
  (if enum
    (get-spec-entity-enum-name (name kebab-case-name) opts)
    (get-spec-entity-name (name kebab-case-name) opts)))

(defmethod get-spec-name :field
  [{:keys [field/kebab-case-name
           field/parent] :as field} opts]
  (get-spec-field-name (name (:type/kebab-case-name parent))
                       (name kebab-case-name)
                       opts))

(defmethod get-spec-name :param
  [param opts]
  (let [type-name (-> param :param/parent :field/parent :type/kebab-case-name)
        field-name (-> param :param/parent :field/kebab-case-name)
        param-name (-> param :param/kebab-case-name)]
    (get-spec-param-name type-name
                         field-name
                         param-name
                         opts)))

(defmethod get-spec-name :param-group
  [params opts]
  (let [type-name (-> params first :param/parent :field/parent :type/kebab-case-name)
        field-name (-> params first :param/parent :field/kebab-case-name)]
    (get-spec-param-group-name type-name
                               field-name
                               opts)))

(defmethod get-spec-name :default
  [obj opts]
  (if (nil? obj)
    nil
    (throw (ex-info "Unable to name a spec for object"
                    {:obj obj}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private get-cardinality [dep-obj]
  (or (:field/cardinality dep-obj)
      (:param/cardinality dep-obj)))

(defn ^:private card-type [dep-obj]
  (let [cardinality (get-cardinality dep-obj)]
    (when cardinality
      (if (and (= 1 (first cardinality))
               (= 1 (second cardinality)))
        :one
        :many))))

(defn ^:private many-cardinality? [dep-obj]
  (= :many (card-type dep-obj)))

(defn ^:private one-cardinality? [dep-obj]
  (= :one (card-type dep-obj)))

(defmulti ^:private get-spec-form*
          (fn [obj opts]
            (cond
              (:field/optional obj)
              :optional-field

              (:param/optional obj)
              :optional-param

              (many-cardinality? obj)
              :many-ref

              (:type/enum obj)
              :enum

              (:type/union obj)
              :union

              (and (:field/name obj)
                   (-> obj :field/parent :type/enum))
              :enum-entry

              (:field/union-type obj)
              :union-field

              (:type/name obj)
              :entity

              (:datomic/type obj)
              (:datomic/type obj)

              (:datomic/tupleType obj)
              :db.type/tuple

              (and (seqable? obj)
                   (every? #(= :param (:node/type %)) obj))
              :param-group

              (:field/name obj)                     ;; simple field, dispatch type name
              (-> obj :field/type :type/name)

              (:param/name obj)                     ;; simple param, dispatch type name
              (-> obj :param/type :type/name))))

(defn ^:private get-spec-form [obj opts]
  (if (map? obj)
    (let [{:keys [spec/override spec/extend spec/gen]} obj
          override' (prepend-core-ns override)
          extend' (prepend-core-ns extend)
          gen' (prepend-core-ns gen)
          target-form (if override'
                        override'
                        (if extend'
                          (list* `s/and [extend' (get-spec-form* obj opts)])
                          (get-spec-form* obj opts)))]
      (if gen'
        (list* `s/with-gen [target-form gen'])
        target-form))
    (get-spec-form* obj opts)))

(defn ^:private get-counts [obj]
  (let [many? (many-cardinality? obj)
        card (get-cardinality obj)
        from (first card)
        to (second card)]
    (when many?
      (cond-> {}
              (= from to)
              (assoc :count from)

              (and (not= from to)
                   (not= 'n from))
              (assoc :min-count from)

              (and (not= from to)
                   (not= 'n to))
              (assoc :max-count to)))))

(defn ^:private get-many-meta-specs [{:keys [spec/distinct spec/kind] :as obj}]
  (let [kind' (prepend-core-ns kind)]
    (cond-> {}
            distinct (assoc :distinct distinct)
            kind (assoc :kind kind'))))

(defmethod get-spec-form* :many-ref
  [obj opts]
  (let [entity-spec (get-spec-form (dissoc obj :field/cardinality :param/cardinality) opts)
        other-nodes (merge (get-counts obj)
                           (get-many-meta-specs obj))]
    (list* `s/coll-of
           (reduce-kv (fn [c k v]
                        (conj c k v))
                      [entity-spec] other-nodes))))
;;EXPLANING:You can use metadata trick Because metadata is on the set item
(defmethod get-spec-form* :enum
  [{:keys [field/_parent]} opts]
  (m/rewrite (mapv #(get-spec-name % opts) _parent)
             [!enums ...] #{^& (!enums ...)}))

(defmethod get-spec-form* :union
  [{:keys [field/_parent]} opts]
  (list* `s/or
         (reduce (fn [c {:keys [field/kebab-case-name] :as field}]
                   (conj c kebab-case-name (get-spec-name field opts)))
                 [] _parent)))
;"No nosso caso, requisitos mais estritos. Tem que ser igual ao tipo"
(defmethod get-spec-form* :enum-entry
  [obj opts]
  (m/rewrite (get-spec-name obj opts)
             ?keyword #(= ?keyword %)))

(defmethod get-spec-form* :union-field
  [{:keys [field/union-type]} opts]
  (get-spec-name union-type opts))

(defmethod get-spec-form* :optional-param
  [obj opts]
  (let [entity-spec (get-spec-form (dissoc obj :param/optional) opts)]
    (list* `s/nilable [entity-spec])))

(defmethod get-spec-form* :optional-field
  [obj opts]
  (let [entity-spec (get-spec-form (dissoc obj :field/optional) opts)]
    (list* `s/nilable [entity-spec])))

(defmethod get-spec-form* :entity
  [{:keys [field/_parent type/implements spec/qualified?] :or {qualified? true}} opts]
  (let [filter-fn (fn [pred c]
                    (->> c
                         (filter :spec/tag)
                         (filter pred)
                         (map #(get-spec-name % opts))
                         vec))
        req (filter-fn #(not (:field/optional %)) _parent)
        opt (filter-fn #(:field/optional %) _parent)
        form (if qualified?
               `(s/keys :req ~req :opt ~opt)
               `(s/keys :req-un ~req :opt-un ~opt))]
    (if implements
      (list* `s/and
             (reduce (fn [c interface]
                       (conj c (get-spec-name interface opts)))
                     [form] implements))
      form)))

;EXTENSOES OCOTOPUS

(defmethod get-spec-form* :db.type/ref
  [obj opts]
  `coll?)

(defmethod get-spec-form* :db.type/string
  [obj opts]
  `string?)

(defmethod get-spec-form* :db.type/keyword
  [obj opts]
  `keyword?)

(defmethod get-spec-form* :db.type/bigint
  [obj opts]
  `(partial instance? clojure.lang.BigInt))

(defmethod get-spec-form* :db.type/long
  [obj opts]
  `(partial instance? java.lang.Long))

(defmethod get-spec-form* :db.type/symbol
  [obj opts]
  `symbol?)

(defmethod get-spec-form* :db.type/bigdec
  [obj opts]
  `decimal?)

(defmethod get-spec-form* :db.type/bytes
  [obj opts]
  `bytes?)

(defmethod get-spec-form* :db.type/uri
  [obj opts]
  `uri?)

(defmethod get-spec-form* :db.type/double
  [obj opts]
  `double?)

(defmethod get-spec-form* :db.type/instant
  [obj opts]
  `inst?)

(defmulti get-spec-form-db-type-tuple
          (fn [{:keys [datomic/tupleAttrs
                       datomic/tupleType
                       datomic/tupleTypes] :as obj} opts]
            (cond tupleAttrs :tuple-composite
                  tupleType :tuple-homogeneous
                  tupleTypes :tuple-heterogeneous)))

(defmethod get-spec-form-db-type-tuple :tuple-composite
  [{:keys [datomic/tupleAttrs] :as obj} opts]
  (m/rewrite tupleAttrs
             [(m/and !n !n1) ...]
             (clojure.spec.alpha/cat . (m/app (comp keyword name) !n)
                                     !n1 ...)))

(defmethod get-spec-form-db-type-tuple :tuple-homogeneous
  [{:keys [datomic/tupleType] :as obj} opts]
  (list `s/coll-of (get-spec-form* {:datomic/type tupleType} {})))


(defmethod get-spec-form-db-type-tuple :tuple-heterogeneous
  [{:keys [datomic/tupleTypes] :as obj} opts]
  (m/rewrite tupleTypes
             [!types ...]
             (clojure.spec.alpha/tuple . (m/app #(get-spec-form* {:datomic/type %} {}) !types)
                                       ...)))

(defmethod get-spec-form* :db.type/tuple
  [obj opts]
  (get-spec-form-db-type-tuple obj opts))

;EXTENSOES OCOTOPUS

(defmethod get-spec-form* :param-group
  [params {:keys [group-type] :as opts}]
  (let [filter-fn (fn [pred c]
                    (->> c
                         (filter pred)
                         (map #(get-spec-name % opts))
                         vec))
        req (filter-fn #(not (:param/optional %)) params)
        opt (filter-fn #(:param/optional %) params)]
    (case group-type
      :map `(s/keys :req-un ~req :opt-un ~opt)
      :tuple (list* `s/tuple (map #(get-spec-name % opts) params)))))

(defmethod get-spec-form* "String" [_ _] `string?)

(defmethod get-spec-form* "ID" [_ _] `uuid?)

(defmethod get-spec-form* "Integer" [_ _] `integer?)

(defmethod get-spec-form* "Boolean" [_ _] `boolean?)

(defmethod get-spec-form* "Float" [_ _] `float?)

(defmethod get-spec-form* "DateTime" [_ _] `inst?)

(defmethod get-spec-form* :default [obj opts]
  (let [ref-type (or (-> obj :field/type)
                     (-> obj :param/type))]
    (if ref-type
      (get-spec-name ref-type opts)
      (throw (ex-info "Unable to create a spec form for object"
                      {:obj obj})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti ^:private pull-node
          (fn [conn node]
            (:node/type node)))

(def ^:private param-selector
  '[*
    {:param/parent [*
                    {:field/parent [*]}]}
    {:param/type [*]}])

(def ^:private field-selector
  `[~'*
    {:param/_parent ~param-selector}
    {:field/parent ~'[*]}
    {:field/type ~'[*]}
    {:field/union-type ~'[*]}])

(def ^:private type-selector
  `[~'* {:type/implements ~'[*]
         :field/_parent   ~field-selector}])

(defmethod pull-node :type
  [conn {:keys [db/id]}]
  (d/pull @conn type-selector id))

(defmethod pull-node :field
  [conn {:keys [db/id]}]
  (d/pull @conn field-selector id))

(defmethod pull-node :param
  [conn {:keys [db/id]}]
  (d/pull @conn param-selector id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:private build-aliases-spec [conn opts]
  (let [eids (-> (q/q '[:find ?e
                        :where
                        [?e :spec/tag true]
                        (or [?e :spec/alias]
                            [?e :spec/aliases])]
                      @conn)
                 vec flatten)
        objs (d/pull-many @conn '[*] eids)]
    (reduce (fn [c {:keys [spec/alias spec/aliases] :as obj}]
              (let [aliases' (or aliases alias)
                    aliases'' (if (seqable? aliases') aliases' [aliases'])
                    node (pull-node conn obj)]
                (into c (map #(hash-map % (get-spec-name node opts))
                             aliases''))))
            [] objs)))

(defn ^:private build-param-group-specs [conn group-type opts]
  (let [eids (-> (q/q '[:find ?f
                        :where
                        [_ :param/parent ?f]
                        [?f :spec/tag true]]
                      @conn)
                 vec flatten)
        objs (d/pull-many @conn '[{:param/_parent [:db/id :node/type]}] eids)]
    (reduce (fn [c {:keys [param/_parent] :as field}]
              (let [nodes (map #(pull-node conn %) _parent)
                    opts' (assoc opts :group-type group-type)]
                (conj c (hash-map (get-spec-name nodes opts')
                                  (get-spec-form nodes opts')))))
            [] objs)))

(defn ^:private build-dummy-types-specs [conn opts]
  (let [eids (get-ids-by-node-type conn :type)
        objs (d/pull-many @conn '[*] eids)]
    (reduce (fn [c obj]
              (conj c (hash-map (get-spec-name obj opts)
                                'any?)))
            [] objs)))

(defn ^:private build-node-spec [conn node opts]
  (-> conn
      (pull-node node)
      (#(hash-map (get-spec-name % opts)
                  (get-spec-form % opts)))))

(defn ^:private build-by-type-specs [conn node-type opts]
  (let [eids (get-ids-by-node-type conn node-type)
        nodes (d/pull-many @conn [:db/id :node/type] eids)]
    (map #(build-node-spec conn % opts) nodes)))

(defn ^:private compile-all
  [conn opts]
  (let [dummy-types-specs (build-dummy-types-specs conn opts)
        params-specs (build-by-type-specs conn :param opts)
        field-specs (build-by-type-specs conn :field opts)
        type-specs (build-by-type-specs conn :type opts)
        aliases-specs (build-aliases-spec conn opts)
        param-groups-specs (build-param-group-specs conn :map opts)
        param-groups-ordered-specs (build-param-group-specs conn :tuple opts)]
    (->> (concat dummy-types-specs
                 params-specs
                 field-specs
                 type-specs
                 aliases-specs
                 param-groups-specs
                 param-groups-ordered-specs)
         (mapv (fn [entry]
                 (let [k (first (keys entry))
                       v (first (vals entry))]
                   #_(do (println " - " k)
                         (println " =>" v)
                         (println " "))
                   `(s/def ~k ~v)))))))

(defn ^:private eval-default-prefix []
  (eval '(str (ns-name *ns*))))

(defn ^:private default-prefix []
  (str (ns-name *ns*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn schema
  ([conn]
   (schema conn nil))
  ([conn {:keys [prefix] :as opts}]
   (let [opts' (if (= :ns prefix) (assoc opts :prefix (default-prefix)) opts)]
     (compile-all conn opts'))))

(defmacro defspecs
  ([conn]
   `(defspecs ~conn nil))
  ([conn {:keys [prefix prefix-entities prefix-enums] :as opts}]
   (let [opts# (if (= :ns prefix) (assoc opts :prefix (eval-default-prefix)) (eval opts))
         opts## (if (= :ns prefix-entities) (assoc opts# :prefix-entities (eval-default-prefix)) opts#)
         opts### (if (= :ns prefix-enums) (assoc opts## :prefix-enums (eval-default-prefix)) opts##)
         conn# (eval conn)]
     (mapv (fn [form] form)
           (schema conn# opts###)))))









  

