(ns battlesnake.server
  (:require [org.httpkit.server :as httpkit]
            [compojure.route :refer [files not-found]]
            [compojure.core :refer [defroutes GET POST DELETE ANY context]]
            [clojure.data.json :as json]
            [ring.middleware.json :refer [wrap-json-body]]))

 (defonce dirs [:left :right :up :down])
(defonce opposites {:left :right
                    :right :left
                    :up :down
                    :down :up})
(defonce snakestate (atom {}))
                    
(defonce server (atom nil))


(defn oob? [pt height width]
  (or (< (:x pt) 0)
      (< (:y pt) 0)
      (>= (:x pt) width)
      (>= (:y pt) height)))

(defn hit? [pt pts]
  (cond
    (empty? pts) false
    (= pt (first pts)) true
    :else (recur pt (rest pts))))

(defn up [pt]
  {:x (:x pt)
   :y (inc (:y pt))})

(defn down [pt]
  {:x (:x pt)
   :y (dec (:y pt))})

(defn left [pt]
  {:x (dec (:x pt))
   :y (:y pt)})

(defn right [pt]
  {:x (inc (:x pt))
   :y (:y pt)})

(defn move-dir [pt dir]
  (cond
    (= dir :right) (right pt)
    (= dir :left) (left pt)
    (= dir :up) (up pt)
    (= dir :down) (down pt)))

(defn move-dirs [pt]
  [(right pt) (left pt) (up pt) (down pt)]
)

(defn can-move3? [you board dir]
  (let [pt (move-dir (:head you) dir)]
    (not (oob? pt (:height board 0) (:width board 0)))))

(defn can-move? [you board dir]
  (let [pt (move-dir (:head you) dir)]
    (not (or (oob? pt (:height board 0) (:width board 0))
             (hit? pt (mapcat :body (remove #(= (:id you) (:id %))
                                            (:snakes board []))))
             (hit? pt (:body you []))))))

(defn can-move4? [you board dir heads]
  (let [pt (move-dir (:head you) dir)]
    (not (or (oob? pt (:height board 0) (:width board 0))
             (hit? pt (mapcat :body (remove #(= (:id you) (:id %))
                                            (:snakes board []))))
             (hit? pt (:body you []))
             (hit? pt heads)))))

(defn get-prev-move [id]
  (get-in @snakestate [id :prev]))

(defn set-prev-move! [id new-move]
  (swap! snakestate (fn [s]
                      (assoc s id {:prev new-move}))))


(defn get-legal-moves [you board]
  (let [snakes (:snakes board [])
        head (:head you)
        legal-dirs dirs]
    (filter #(can-move? you board %) legal-dirs)))

(defn get-best-moves [you prev-dir board]
  (assert (not (nil? prev-dir)))
  (let [dirs (get-legal-moves you prev-dir board)
        coll-or-prev (fn [coll]
                       (if (empty? coll)
                         [prev-dir]
                         coll))]
    (coll-or-prev(filter (fn [d]
                           (let [head (move-dir (:head you) d)
                                 body (concat head (butlast (:body you)))
                                 you (assoc you
                                            :head head
                                            :body body)]
                             (if (empty? (get-legal-moves you d board))
                               false
                               true)))
                         (remove nil? dirs)))))

(defn respond [m]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (json/write-str m)})


(defn index [req]
  (respond  {:apiversion "1"
             :author "Nick Gonzalez"
             :color "#ed40d0"
             :head "tiger-king"
             :tail "tiger-tail"
             :version "0.0.1-beta"}))

(defn start [req]
  (def body (:body req))
  (reset! snakestate {})
  (respond {}))

(defn end [req]
  (respond {}))

(defn move2 [req]
  (let [gamestate (:body req)
        id (get-in gamestate [:you :id])
        prev (get-prev-move id)
        legal-moves (get-legal-moves (:you gamestate) (:board gamestate))
        update-state-and-respond (fn [d]
                                   (set-prev-move! id d)
                                   (respond {:move d}))]
    (if (empty? legal-moves)
      (do (println "No moves. Die.")
          (update-state-and-respond prev))
      (update-state-and-respond (rand-nth legal-moves)))))

(defn move-snake [snake dir grow]
  (let [{head :head
         body :body} snake
        new-head (move-dir head dir)]
    (assoc snake
           :head new-head
           :body (vec (cons new-head (if grow body (butlast body)))))))

(defn has-food? [pt board]
  (> (count (filter (partial = pt) (:food board))) 0))

(defn update-health [snake board]
  (let [health (:health snake 100)]
    (assoc snake :health (if (has-food? (:head snake) board)
                           100
                           (dec health)))))

(defn score-move-recursive
  "Returns a score for this move."
  [snake board max-depth dir]
  (let [new-snake (move-snake snake dir false)
        legal-moves (get-legal-moves new-snake board)
        local-score (count legal-moves)
        depth (dec max-depth)]
    (cond
      (empty? legal-moves) 0
      (zero? depth) local-score
      :else
      (reduce + local-score
              (map (partial score-move-recursive new-snake board depth)
                   legal-moves)))))

(defn score-move-recursive2
  "Returns a score for this move."
  [snake board max-depth dir]
  (let [valid-dir? (can-move? snake board dir)
        new-snake (if (has-food? (move-dir (:head snake) dir) board)
                    (update-health (move-snake snake dir true) board)
                    (update-health
                     (move-snake snake dir false) board)) 
        depth (dec max-depth)]
    (cond
      (not valid-dir?) 0
      (zero? (:health new-snake 100)) 0 
      (zero? depth) (- 100 (:health new-snake 100))
      :else
      (let [score (score-move-recursive2 new-snake board depth :right)]
        (if (> score 0)
          score
          (let [score (score-move-recursive2 new-snake board depth :left)]
            (if (> score 0)
              score
              (let [score (score-move-recursive2 new-snake board depth :down)]
                (if (> score 0)
                  score
                  (let [score (score-move-recursive2 new-snake board depth :up)]
                    (if (> score 0)
                      score
                      0)))))))))))

(defn score-move-recursive3
  "Returns a score for this move."
  [snake board max-depth dir]
  (let [valid-dir? (can-move? snake board dir)
        new-snake (if (has-food? (move-dir (:head snake) dir) board)
                    (update-health (move-snake snake dir true) board)
                    (update-health
                     (move-snake snake dir false) board)) 
        depth (dec max-depth)]
    (cond
      (not valid-dir?) 0
      (zero? (:health new-snake 100)) 0 
      (zero? depth) (- 100 (:health new-snake 100))
      :else
      (let [score (score-move-recursive2 new-snake board depth :right)]
        (if (> score 0)
          score
          (let [score (score-move-recursive2 new-snake board depth :left)]
            (if (> score 0)
              score
              (let [score (score-move-recursive2 new-snake board depth :down)]
                (if (> score 0)
                  score
                  (let [score (score-move-recursive2 new-snake board depth :up)]
                    (if (> score 0)
                      score
                      0)))))))))))
  
(defn score-move-recursive4
  "Returns a score for this move."
  [snake board max-depth dir heads]
  (let [valid-dir? (can-move4? snake board dir heads)
        new-snake (if (has-food? (move-dir (:head snake) dir) board)
                    (update-health (move-snake snake dir true) board)
                    (update-health
                     (move-snake snake dir false) board))
        new-heads (if (>= (count heads) 12) (distinct (flatten (map move-dirs heads))) heads)
        depth (dec max-depth)]
    (println new-heads)
    (cond
      (not valid-dir?) 0
      (zero? (:health new-snake 100)) 0 
      (zero? depth) (- 100 (:health new-snake 100))
      :else
      (let [score (score-move-recursive4 new-snake board depth :right new-heads)]
        (if (> score 0)
          score
          (let [score (score-move-recursive4 new-snake board depth :left new-heads)]
            (if (> score 0)
              score
              (let [score (score-move-recursive4 new-snake board depth :down new-heads)]
                (if (> score 0)
                  score
                  (let [score (score-move-recursive4 new-snake board depth :up new-heads)]
                    (if (> score 0)
                      score
                      0)))))))))))

(def ^:dynamic *max-depth* 10)

(def score-move-recursive-fast
  (memoize (fn [snake board max-depth dir]
             (score-move-recursive snake board max-depth dir))))
(defn think
  "return a map of directions to scores. High score wins."
  [my-snake board & {:keys [depth]}]
  (let [score-move (fn [snake board dir]
                     (if (can-move? snake board dir)
                       1.0 0.0))
        max-depth (if (nil? depth)
                    *max-depth*
                    depth)
        heads (map :head (:snakes board []))
        score score-move-recursive4]
    {:up (score my-snake board max-depth :up heads)
     :down (score my-snake board max-depth :down heads)
     :right (score my-snake board max-depth :right heads)
     :left (score my-snake board max-depth :left heads)}))

(defn think-clearly [snake board]
  (let [thoughts (think snake board)]
    (if (empty? (remove zero? (vals thoughts)))
      (do
        (println "Hope is lost.")
        (think snake board :depth 1))
      thoughts)))

(defn process-thoughts
  "return a direction"
  [thoughts]
  (when-not (empty? thoughts)
    (first (last (sort-by second (vec thoughts))))))

(defn move-intently
  "think before you act"
  [req]
  (let [gamestate (:body req)
        id (get-in gamestate [:you :id])
        prev (get-prev-move id)
        thoughts (think (:you gamestate) (:board gamestate))
        _ (println thoughts)
        update-state-and-respond (fn [d]
                                   (set-prev-move! id d)
                                   (respond {:move d}))]
    (if (empty? thoughts)
      (do (println "No moves. Die.")
          (update-state-and-respond prev))
      (update-state-and-respond (process-thoughts thoughts)))))

(defn move3 [req]
  (let [gamestate (:body req)
        id (get-in gamestate [:you :id])
        prev (get-prev-move id)
        
        legal-moves (get-legal-moves (:you gamestate) (:board gamestate))
        update-state-and-respond (fn [d]
                                   (set-prev-move! id d)
                                   (respond {:move d}))]
    (if (empty? legal-moves)
      (do (println "No moves. Die.")
          (update-state-and-respond prev))
      (update-state-and-respond (rand-nth legal-moves)))))

(defn move [req]
  (println (:you (:body req)))
  (let [resp (move-intently req)]
    (println resp)
    resp))

(defroutes all-routes
  (GET "/" [] index)
  (POST "/start" [] start)
  (POST "/move" [] move)
  (POST "/end" [] end)
  (not-found "<p>Page not found.</p>"))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn log-request [req]
  (clojure.pprint/pprint {:type :request
                          :method (:request-method req)
                          :uri (:uri req)
                          :body (:body req)}))

(defn log-response [resp]
  (clojure.pprint/pprint {:type :response
                          :body (:body resp)}))

(defn wrap-with-logging [handler]
  (fn [req]
    (log-request req)
    (log-response (handler req))))

(def app
  (-> all-routes
      (wrap-json-body {:keywords? true})
      ))

(defn run []
  (reset! server (httpkit/run-server #'app {:port 80})))
