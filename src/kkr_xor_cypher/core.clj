(ns kkr-xor-cypher.core
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:require [clojure.string :as string])
  (:gen-class))

(def alphabet "АБВГҐДЕЄЖЗИІЇЙКЛМНОПРСТУФХЦЧШЩЬЮЯ_0123456789")

(defn in-alphabet? [text]
  (every? #(contains? (set alphabet) %) text))

(def cli-options
  [["-k" "--key KEY" "Key to use for encryption or decryption."
    :id :key
    :parse-fn #(str %)
    :validate [#(not (empty? %)) "A key must be provided"]]
   ["-t" "--text TEXT" "A text to encrypt/decrypt"
    :id :text
    :parse-fn #(str %)
    :validate [
      #(and (not (empty %)) (in-alphabet? %))
        (string/join ["A text must be provided and consist of next characters: " alphabet] "")]]
   ;; A boolean option defaulting to nil
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Usage: lein run [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  encrypt    Encrypt text"
        "  decrypt    Decrypt text"]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-args
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      ;; custom validation on arguments
      (and (= 1 (count arguments))
           (#{"encrypt" "decrypt"} (first arguments)))
      {:action (first arguments) :options options}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))


(defn encrypt-char [char key]
  (let [char-index (.indexOf alphabet (int char))
        key-index (.indexOf alphabet (int key))]
    (if (and (>= char-index 0) (>= key-index 0))
      (.charAt alphabet (mod (+ char-index key-index) (count alphabet)))
      char)))

(defn decrypt-char [char key]
  (let [char-index (.indexOf alphabet (int char))
        key-index (.indexOf alphabet (int key))]
    (if (and (>= char-index 0) (>= key-index 0))
      (.charAt alphabet (mod (+ (- char-index key-index) (count alphabet)) (count alphabet)))
      char)))

(defn encrypt-text [text key]
  (apply str (mapv (fn [c k] (encrypt-char c k)) text (cycle key))))

(defn decrypt-text [text key]
  (apply str (mapv (fn [c k] (decrypt-char c k)) text (cycle key))))

(defn -main [& args]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "encrypt" (println (encrypt-text (:text options) (:key options)))
        "decrypt" (println (decrypt-text (:text options) (:key options)))))))
