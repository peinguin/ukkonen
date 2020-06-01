(defstruct node :start :end :branches :link)
(defstruct point :node :length)

(defn apply-to-node [f path tree]
  (if (= (count path) 0)
    (f tree)
    (let
      [[head & tail] path]
			(assoc tree :branches (vec (map-indexed
					(fn [i branch]
						(if (= i head)
							(apply-to-node f tail branch)
							branch))
					(tree :branches)))))))

(defn find-node [path tree]
	(if (= (count path) 0)
		tree
		(let
			[[head & tail] path]
			(recur tail (nth (tree :branches) head)))))

(defn get-symbol-edge-by-branch [c node string]
	(first ;; Take the index
		(first
			(filter
				(fn [[_ {start :start}]] (= (subs string start (+ start 1)) c))
				(map-indexed vector (node :branches))))))

(defn get-symbol-edge [c active tree string]
	(get-symbol-edge-by-branch
		c
		(find-node (active :node) tree)
		string))

(defn add-a-branch [parent start]
	(if (or (not (parent :end)) (< (parent :end) start))
		(struct-map node ;; just add a branch
			:start (parent :start)
			:end (if (parent :end) (min start (parent :end)) start)
			:branches (conj
				(parent :branches)
				(struct-map node
					:start start
					:end nil
					:branches []
					:link nil))
			:link nil)
		(struct-map node ;; move branches to the new node
			:start (parent :start)
			:end start
			:branches [(assoc parent :start start)]
			:link nil)))

(defn split-branch-fn [current active reminder]
  (fn [branch]
		(let
			[reminder-start (+ current reminder)]
			(if (= (active :length) 0)
				(add-a-branch branch reminder-start)
				(add-a-branch (add-a-branch branch (+ (branch :start) (active :length))) reminder-start)))))

(defn get-link [active tree]
	(let
		[
			base-node-path (if (active :length)
				(if (= (count (active :node)) 0)
					nil
					(pop (active :node)))
				(active :node))
		]
		(if base-node-path ((find-node base-node-path tree) :link) nil)))

(defn normalize-active [active tree string]
	(if (= (active :length) 0)
		active
		(let
			[node (find-node (active :node) tree)]
			(if (and (node :end) (>= (+ (node :start) (active :length)) (node :end)))
				(let
					[
						c-pos (node :end)
						edge (get-symbol-edge-by-branch (subs string c-pos (+ c-pos 1)) node string) ;; May be replaced by the first branch
					]
					(recur
						(assoc
							active
							:node (conj (active :node) edge)
							:length (- (active :length) (- (node :end) (node :start))))
							tree
							string))
				active))))

(defn split-branch [last-inserted-node current active reminder tree string]
  (let
		[
			link (get-link active tree)
			next-reminder (- reminder 1)
			next-current (+ current 1)
			splitted-branch-tree (apply-to-node
				(split-branch-fn current active reminder)
				(active :node)
				tree)
			splitted-node (if (= (active :length) 0) nil (active :node))
			new-tree (if (and splitted-node last-inserted-node)
				(apply-to-node ;; link the prev created node
					(fn [branch] (assoc branch :link splitted-node))
					last-inserted-node
					splitted-branch-tree)
				splitted-branch-tree) ;; do not
			active-node (find-node (active :node) new-tree)
		]
		(if (< next-reminder 1)
			new-tree ;; the split is over
			(if (< (count (active :node)) 2) ;; continue splitting
				(let ;; did insert to root
					[
						new-active-length (- (active :length) 1)
						c-pos (+ (active-node :start) 1)
						c (subs string c-pos (+ c-pos 1))
						edge (get-symbol-edge c (struct-map point :node [] :length 0) new-tree string)
					]
					(recur
						splitted-node
						next-current
						(normalize-active (assoc active :node [edge] :length new-active-length) new-tree string)
						next-reminder
						new-tree
						string))
				(let
					[c (subs string (active-node :start) (+ (active-node :start) 1))]
					(if link ;; did insert to node
						(let ;; go to the link
							[
								node (if (> (active :length) 0)
									(conj link (get-symbol-edge c (struct-map point :node link :length 0) new-tree string)) ;; splitting the branch
									link) ;; adding a branch
							]
							(recur
								splitted-node
								next-current
								(normalize-active (assoc active :node node) tree string)
								next-reminder
								new-tree
								string))
						(let ;; go to the root
							[
								node (if (> (active :length) 0)
									[(get-symbol-edge c (struct-map point :node [] :length 0) new-tree string)] ;; splitting the branch
									[]) ;; adding a branch
							]
							(recur
								splitted-node
								next-current
								(normalize-active (assoc active :node node) new-tree string)
								next-reminder
								new-tree
								string))))))))

(defn get-substring [node tree string]
	(let
		[tree (find-node node tree)]
		(subs
			string
			(tree :start)
			(if (tree :end) (tree :end) (count string)))))

(defn go-further [current active reminder tree string]
	(let
		[
			real-current (+ current reminder)
			real-end (+ real-current 1)
			end-of-string (>= real-end (count string))
		]
		(if (end-of-string)
			nil
			(let
				[c (subs string real-current real-end)]
				(if (active :length)
					(let
						[
							node (find-node (active :node) tree)
							next-length (+ (active :length) 1)
							next-c-pos (+ (node :start) next-length)
						]
						(if (= c (subs string next-c-pos (+ next-c-pos 1)))
							(normalize-active (assoc active :length next-length) tree string)
							nil))
					(let
						[edge (get-symbol-edge c active tree string)]
						(if edge
							(struct-map point :node (conj (active :node) edge) active :length 1)
							(assoc active :length 0))))))))

(defn ukkonen-step [current active reminder tree string]
(print "UKKONEN-STEP" current active reminder string "\n")
(pprint tree)
	(if (= current (count string))
		tree ;; the sting is processed
		(let ;; process is in progress
			[next-active (go-further current active reminder tree string)]
			(if next-active
				(recur
					current
					next-active
					(+ reminder 1)
					tree
					string
				)
				(recur
					(+ current reminder 1)
					(struct-map point :node [] :length 0)
					0
					(split-branch nil current active reminder tree string)
					string)))))

(defn ukkonen [string]
  (def tree (struct-map node :start 0 :end nil :branches [] :link nil))
  (ukkonen-step
    0
    (struct-map point :node [] :length 0)
    0
    tree
    string))

(defn print-all-the-substrings
	([string] (print-all-the-substrings "" (ukkonen string) string) nil)
	([prefix tree string]
		(let
			[curr-prefix (str prefix (subs string (tree :start) (if (tree :end) (tree :end) (count string))))]
			(if (= (count (tree :branches)) 0)
				(print curr-prefix "\n")
				(doall (map #(print-all-the-substrings curr-prefix % string) (tree :branches)))))))

(print-all-the-substrings "abcabxabcd")
;;(print-all-the-substrings "aaaaaa")
;;(print-all-the-substrings "abcabxabcd")
;;(print-all-the-substrings "abcdeabcqdebcdfabcdef")

(defn number-occurs ;; works only with substrings
	([substr string] (number-occurs 1 1 substr string))
	([pos res substr string]
		(let
			[end (+ pos (count substr))]
			(if (> end (count string))
				res
				(recur
					(+ pos 1)
					(if (= substr (subs string pos end)) (+ res 1) res)
					substr
					string)))))

(defn is-polindrome [string]
	(let
		[half (quot (count string) 2)]
		(=
			(subs string 0 half)
			(apply str (reverse (subs string (- (count string) half) (count string)))))))

(defn f [substr string]
	(if (is-polindrome substr)
		(* (count substr) (number-occurs substr string))
		0)) ;; it is not true, but smaller than string length

(defn get-next-leaf [path prefix tree string]
	(let
		[
			parent-path (pop path)
			branch (last path)
			parent-node (find-node parent-path tree)
		]
		(if (and (= (count parent-path) 0) (= (count (parent-node :branches)) branch))
			(list nil nil)
			(if (> (count (parent-node :branches)) branch)
				(list prefix path)
				(let
					[
						grandparent-path (pop parent-path)
						parent-branch (last parent-path)
					]
					(recur
						(conj grandparent-path (+ parent-branch 1))
						(subs
							prefix
							0
							(-
								(count prefix)
								(-
									(if (parent-node :end) (parent-node :end) (count string))
									(parent-node :start))))
						tree
						string))))))

(defn max-value
	([string ](max-value [0] "" (count string) (ukkonen string) string))
	([path prefix f-max tree string]
		(let
			[
				substr (str prefix (get-substring path tree string))
				f-local (f substr string)
				f-max-local (max f-local f-max)
				[path-next prefix-next] (get-next-leaf (conj path 0) substr tree string)
			]
			(if path-next
				(recur prefix-next path-next f-max-local tree string)
				f-max-local))))

;;(print "RESULT: " (max-value "abcabcddd") "\n")
;;(print "RESULT: " (max-value "aaaaaa") "\n")
;;(print "RESULT: " (max-value "abcdeabcqdebcdfabcdef") "\n")
;;(print "RESULT: " (max-value "abcabxabcd") "\n")

;;(def fptr (get (System/getenv) "OUTPUT_PATH"))(def t (read-line))(print (max-value t) "\n")
