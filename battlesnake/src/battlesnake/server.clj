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


(def oob? (memoize (fn [pt height width]
                     (or (< (:x pt) 0)
                         (< (:y pt) 0)
                         (>= (:x pt) width)
                         (>= (:y pt) height)))))

(defn hit? [pt pts]
  (cond
    (empty? pts) false
    (= pt (first pts)) true
    :else (recur pt (rest pts))))

(def fast-hit? (memoize hit?))

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

(defn move-dirs [pt board]
  (let [left-move (if
          (not (oob? (left pt) (:height board 0) (:width board 0)))
          (left pt)
          nil)
        right-move (if
          (not (oob? (right pt) (:height board 0) (:width board 0)))
          (right pt)
          nil)
        up-move (if
          (not (oob? (up pt) (:height board 0) (:width board 0)))
          (up pt)
          nil)
        down-move (if
          (not (oob? (down pt) (:height board 0) (:width board 0)))
          (down pt)
          nil)]
    (remove nil? [left-move right-move up-move down-move])))

(def move-dirs-fast (memoize move-dirs))

(defn move-dirs-board [board]
   (fn [pt] (move-dirs pt board)))

(defn can-move3? [you board dir]
  (let [pt (move-dir (:head you) dir)]
    (not (oob? pt (:height board 0) (:width board 0)))))

(defn can-move? [you board dir]
  (let [pt (move-dir (:head you) dir)]
    (not (or (oob? pt (:height board 0) (:width board 0))
             (fast-hit? pt (mapcat :body (remove #(= (:id you) (:id %))
                                            (:snakes board []))))
             (fast-hit? pt (:body you []))))))

(defn can-move4? [you board dir heads]
  (let [pt (move-dir (:head you) dir)]
    (not (or (oob? pt (:height board 0) (:width board 0))
             (fast-hit? pt (mapcat :body (remove #(= (:id you) (:id %))
                                            (:snakes board []))))
             (fast-hit? pt (:body you []))
             (fast-hit? pt heads)))))

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
             :version "0.0.1"}))

(defn start [req]
  (def body (:body req))
  (reset! snakestate {})
  (respond {}))

(defn end [req]
  (prn (:body req))
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
  (let [new-heads (if (< (count heads) 8)
                    (distinct (concat
                               (flatten (map
                                         (move-dirs-board board)
                                         heads))
                               heads))
                    heads)
        valid-dir? (can-move4? snake board dir new-heads)
        new-snake (if (has-food? (move-dir (:head snake) dir) board)
                    (update-health (move-snake snake dir true) board)
                    (update-health
                     (move-snake snake dir false) board))
        depth (dec max-depth)]

    (cond
      (not valid-dir?) {:depth depth
                        :health (:health new-snake 100)}
      (zero? (:health new-snake 100))  {:depth depth
                                        :health (:health new-snake 100)}
      (zero? depth) {:depth depth
                     :health (:health new-snake 100)}
      :else
      (let [score (score-move-recursive4 new-snake board depth :right new-heads)]
        (if (> (:depth score) 0)
          score
          (let [score (score-move-recursive4 new-snake board depth :left new-heads)]
            (if (> (:depth score) 0)
              score
              (let [score (score-move-recursive4 new-snake board depth :down new-heads)]
                (if (> (:depth score) 0)
                  score
                  (let [score (score-move-recursive4 new-snake board depth :up new-heads)]
                    (if (> (:depth score) 0)
                      score
                      {:depth depth
                       :health (:health new-snake)})))))))))))

(defn get-legal-moves-with-heads [pt board heads]
  (map (partial move-dir pt)
       (filter #(can-move4? {:head pt} board % heads)
               dirs)))

(def get-new-heads (memoize (fn [heads-map height width]
                              (reduce (fn [accum pt]
                                        (let [c (get accum pt 0)]
                                          (assoc accum pt (inc c))))
                                      heads-map
                                      (mapcat #(move-dirs-fast % {:height height
                                                            :width width})
                                              (keys heads-map))))))

(defn init-snake-probability-map [body]
  (assoc (reduce (fn [accum pt]
                   (assoc accum
                          pt 1.0))
                 {}
                 body)
         (first body) -1.0))

(def update-snake-probability-map
(fn [prob-map height width]
             (let [heads (map first (filter #(neg? (second %))
                                            (vec prob-map)))
                   add-new (fn [accum pt]
                             (let [pts (filter #(zero? (get accum % 0))
                                               (move-dirs-fast pt {:height height
                                                                   :width width}))
                                   pts-len (count pts)
                                   head-prob (- (get accum pt 0))
                                   pt-prob (if (zero? pts-len)
                                             0
                                             (- (*
                                                 (/ 1.0 pts-len)
                                                 head-prob)))]
                               (assoc (reduce (fn [accum2 pt2]
                                                (let [prob (get accum2 pt 0)]
                                                  (assoc accum2
                                                         pt2
                                                         pt-prob)))
                                              accum
                                              pts)
                                      pt head-prob)))]
               (reduce add-new prob-map heads))))

(defn update-all-snake-probability-maps [snake-probs height width]
  (map #(update-snake-probability-map % height width)
       snake-probs))

(defn init-all-snake-probability-maps [bodies height width]
  (map #(update-snake-probability-map (init-snake-probability-map %) height width)
       bodies))

(def
  count-same (memoize (fn [pt pts]
                        (count (filter (partial = pt) pts)))))

(defn snakes-not-me [snake snakes]
  (remove #(= (:id snake) (:id %))
          snakes))


(def ^:dynamic *max-depth* 10)
(defn score-move-recursive6
  "Returns a score for this move."
  [snake board max-depth dir snake-probs prob start-time]
  (let [valid-dir? (can-move? snake board dir)
        new-snake (if (has-food? (move-dir (:head snake) dir) board)
                    (update-health (move-snake snake dir true) board)
                    (update-health
                     (move-snake snake dir false) board))
        depth (dec max-depth)]
    (cond
      (not valid-dir?) 0
      (zero? (:health new-snake 100)) 0
      :else
      (let [snake-probs (if (> depth 0)
                          (update-all-snake-probability-maps snake-probs
                                                             (:height board)
                                                             (:width board))
                          snake-probs)
            hit-probability (if (empty? snake-probs)
                              0.0
                              (apply max (conj (map #(get % (:head new-snake) 0.0) snake-probs)
                                               0.0)))
            snakes (snakes-not-me new-snake (:snakes board))
            max-len (if (zero? (count snakes))
                      0
                      (apply max (map :length  snakes)))
            len-diff (- (:length new-snake) max-len)
            hit-probability (+ hit-probability prob)
            hit-weight (- 1.0 hit-probability)
            threshold 99
            elapsed-time (/ (- (System/nanoTime) start-time ) 1000000)]
        (cond
          (> elapsed-time 200) 0
          (> hit-probability 0.5) 0
          (zero? depth) (if (< len-diff 3)
                          (count (:body new-snake))
                          hit-weight)
          
          :else
            (apply max (map #(score-move-recursive6 new-snake board depth % snake-probs hit-probability start-time)
                             (shuffle [:left :right :up :down]))))))))


(defn think
  "return a map of directions to scores. High score wins."
  [my-snake board & {:keys [depth]}]
  (let [score-move (fn [snake board dir]
                     (if (can-move? snake board dir)
                       1.0 0.0))
        max-depth (if (nil? depth)
                    *max-depth*
                    depth)
        bodies (map :body (snakes-not-me my-snake (:snakes board [])))
        snake-probabilities (init-all-snake-probability-maps bodies
                                                             (:height board)
                                                             (:width board))
        score score-move-recursive6
        start-time (System/nanoTime)
        
        scores {:up (future (score my-snake board max-depth :up snake-probabilities 0.0 start-time))
                :down (future (score my-snake board max-depth :down snake-probabilities 0.0 start-time))
                :right (future (score my-snake board max-depth :right snake-probabilities 0.0 start-time))
                :left (future (score my-snake board max-depth :left snake-probabilities 0.0 start-time))}]
    {:up @(:up scores)
     :down @(:down scores)
     :right @(:right scores)
     :left @(:left scores)}))

(defn think-test
  "return a map of directions to scores. High score wins."
  [my-snake board & {:keys [depth]}]
  (let [score-move (fn [snake board dir]
                     (if (can-move? snake board dir)
                       1.0 0.0))
        max-depth (if (nil? depth)
                    *max-depth*
                    depth)
        bodies (map :body (snakes-not-me my-snake (:snakes board [])))
        snake-probabilities (init-all-snake-probability-maps bodies
                                                             (:height board)
                                                             (:width board))
        score score-move-recursive6
        start-time (System/nanoTime)]
    (score my-snake board max-depth :down snake-probabilities 0.0 start-time)))

(defn think-clearly [snake board]
  (loop [depth  *max-depth*]
    (let [thoughts (think snake board :depth (or depth *max-depth*))
          zero-or-one (fn [x] (if x 1 0))
          score (fn [dir]
                  (zero-or-one (can-move? snake board dir)))]
      (cond
        (not (empty? (remove zero? (vals thoughts)))) thoughts
        (zero? depth)  {:left (score :left)
                        :right (score :right)
                        :down (score :down)
                        :up (score :up)}
        :else (do
                (println "Hope is lost.")
                (recur (dec depth)))))))

(defn think-more-clearly [snake board]
  (let [thoughts (think snake board :depth *max-depth*)
        zero-or-one (fn [x] (if x 1 0))
        score (fn [dir]
                (zero-or-one (can-move? snake board dir)))]
    (cond
      (not (empty? (remove zero? (vals thoughts)))) thoughts
      :else (do
              (println "Hope is lost.")
              {:left (score :left)
               :right (score :right)
               :down (score :down)
               :up (score :up)}))))
(defn process-thoughts
  "return a direction"
  [thoughts]
  (when-not (empty? thoughts)
    (first (last (sort-by second (vec thoughts))))))

(defn process-thoughts-better
  "return a direction"
  [thoughts]
  (when-not (empty? thoughts)
    (first (first (reverse (sort-by (fn [x]
                                      (let [health (:health (second x))]
                                        (if (> health 50)
                                          (- 100 health 50)
                                          health)))
                                    (second (first (reverse
                                                    (sort-by first
                                                             (group-by #(:depth (second %)) thoughts)))))))))))

(defn move-intently
  "think before you act"
  [req]
  (let [gamestate (:body req)
        id (get-in gamestate [:you :id])
        prev (get-prev-move id)
        thoughts (think-more-clearly (:you gamestate) (:board gamestate))
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
  (prn (:body req))
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

