(ns octopus-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [hodur-engine.octopus :as engine]
            [hodur-spec-schema.octopus :as hodur-spec]
            [com.rpl.specter :refer :all]
            [midje.sweet :refer :all]
            [matcher-combinators.midje :refer [match throws-match]]
            [matcher-combinators.matchers :as mt]
            [clojure.set :as set]
    #_[sc.api :as sc])
  (:import (java.util UUID)
           (java.net URI)))

(def meta-db (engine/init-schema '[^{:spec/tag                   true
                                     :model.attr/apenas-runtime? false
                                     :optional                   true}


                                   default


                                   ^{:interface true}
                                   Workflow
                                   [^{:type                                 Estados-Workflow, :cardinality 1, :optional false
                                      :model.attr/persiste-estado-workflow? true
                                      :doc                                  "Estado da máquina de estados associada à entidade"} status
                                    ^{:type String, :cardinality 1, :optional false,
                                      :doc  "Identificador do workflow - namespace completo"} ident]

                                   ^{:interface true}
                                   Person
                                   [^{:type                       String
                                      :model.attr/apenas-runtime? false}
                                    name
                                    ^{:type           ID
                                      :datomic/unique :db.unique/value}
                                    id
                                    ^Estado-Workflow-Person status]

                                   Employee
                                   [^{:type     ID
                                      :optional false}
                                    id
                                    ^String name

                                    ^{:type             String
                                      :doc              "The very employee number of this employee"
                                      :datomic/unique   :db.unique/identity
                                      :optional         false
                                      :datomic/fulltext false}
                                    number
                                    ^Float salary
                                    ^Integer age
                                    ^DateTime start-date
                                    ^Employee supervisor
                                    ^{:type        Employee
                                      :cardinality [0 n]
                                      :doc         "Has documentation"
                                      :deprecation "But also deprecation"}
                                    co-workers
                                    ^{:datomic/type               :db.type/keyword
                                      :model.attr/apenas-runtime? true}
                                    keyword-type
                                    ^{:datomic/type :db.type/uri}
                                    uri-type
                                    ^{:datomic/type :db.type/double}
                                    double-type
                                    ^{:datomic/type :db.type/long}
                                    long-type

                                    ^{:datomic/type :db.type/long}
                                    long-type

                                    ^{:datomic/type :db.type/bigint}
                                    bigint-type

                                    ^{:datomic/type :db.type/symbol}
                                    symbol-type

                                    ^{:datomic/type :db.type/bigdec
                                      :deprecation  "This is deprecated"}
                                    bigdec-type
                                    ^{:datomic/type          :db.type/tuple
                                      :datomic/tupleType     :db/long
                                      :cardinality           [1 n]
                                      :optional              true
                                      :model.attr/persisted? true} tupla-simples

                                    ^{:datomic/type          :db.type/tuple
                                      :datomic/tupleTypes    [:db/long :db/keyword]
                                      :optional              true
                                      :model.attr/persisted? true} tupla-composta

                                    ^{:datomic/type       :db.type/tuple
                                      :datomic/tupleAttrs [:employee/age :employee/co-workers]
                                      :optional           true
                                      :cardinality        1
                                      :doc                "Identificador entidade composta"}
                                    composite-key
                                    ^EmploymentType employment-type
                                    ^Estado-Workflow-Employee status
                                    ^SearchResult last-search-results]

                                   ^{:union true
                                     :spec/alias :entidade/search-result}
                                   SearchResult
                                   [Employee Person EmploymentType]

                                   ^{:enum               true
                                     :model.attr/dominio :enum/teste}
                                   EmploymentType
                                   [FULL_TIME
                                    ^{:doc "Documented enum"}
                                    PART_TIME]
                                   ^{:enum               true
                                     :model.attr/dominio :enum/employment-type}
                                   EmploymentType
                                   [FULL_TIME
                                    ^{:doc "Documented enum"}
                                    PART_TIME]

                                   ^{:enum               true
                                     :model.attr/dominio :enum/estado-workflow.person}
                                   Estado-Workflow-Person
                                   [ACEITO]

                                   ^{:enum               true
                                     :model.attr/dominio :enum/estado-workflow.employee}
                                   Estado-Workflow-Employee
                                   [ACEITO]

                                   ^{:enum               true
                                     :union              true
                                     :model.attr/dominio :enum/estados-workflow}
                                   Estados-Workflow
                                   [Estado-Workflow-Employee Estado-Workflow-Person]]))

(def register-spec (hodur-spec/defspecs meta-db))
(deftest tests-specs-octopus

  (fact ":db/type keyword"
        (s/valid? :employee/keyword-type :teste/keyoerd) => truthy)

  (fact ":db.type/bigdec"
        (s/valid? :employee/bigdec-type (bigdec 5)) => truthy)

  (fact ":db.type/double"
        (s/valid? :employee/double-type (double 5)) => truthy)

  (fact ":db.type/bigint"
        (s/valid? :employee/bigint-type (bigint 5)) => truthy)

  (fact ":db.type/long"
        (s/valid? :employee/long-type (long 5)) => truthy)

  (fact ":db.type/uri"
        (s/valid? :employee/uri-type (URI. "http://example.com/foo/bar")) => truthy)

  (fact ":db.type/symbol?"
        (s/valid? :employee/symbol-type 'symbol) => truthy)

  (facts "ID - na versao ocotopus agora valida como uuid"
         (fact
           (s/valid? :employee/id (.toString (UUID/randomUUID))) => falsey)

         (fact
           (s/valid? :employee/id (UUID/randomUUID))) => truthy)

  (facts "Tuplas composite"
         (fact
           (s/valid? :employee/composite-key [1 [{:employee/id (UUID/randomUUID) :employee/number "number" :employee/name "Nome CVoworker"}]]) => truthy)

         (fact
          (s/valid? :employee/composite-key [1 [{:employee/id (.toString (UUID/randomUUID)) :employee/number "number" :employee/name "Nome CVoworker"}]]) => falsey)

         (fact
           (s/valid? :employee/composite-key [1 {:employee/id (.toString (UUID/randomUUID)) :employee/number "number" :employee/name "Nome CVoworker"}]) => falsey)))
