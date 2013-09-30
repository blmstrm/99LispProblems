;;; Particle Swarm Optimization
;;;
;;; For each particle i=1,...,S do:
;;;  * Initialize the particle's position with a uniformly distributed random vector: x_i~U(b_lo,b_up) where b_lo and b_up are
;;;   the lower and upper boundaries of the search space.
;;;  * Initialize the particle's best known position to its initial position: p_i <- x_i
;;;  * If f(p_i) < f(g) update th swarm's best known position: g <- p_i
;;;  * Initialize the particle's velocity: v_i ~U(-|b_up - b_lo|,|b_up-b_lo|)
;;;  
;;;  * Until a termination criterion is met (e.g. number of iterations performed, or a solution with adequate objective function value is found), repeat:
;;;   * For each particle i = 1,...,S do:
;;;     * Pick random numbers: r_p,r_g ~U(0,1)
;;;     * For each dimension d= 1,...,n do:
;;;       * Update the particle's velocity: v_(i,d) <- stjärtw v_(i,d) + svansp_p * r_p * (p_(i,d)-x_(i,d)) + psvans_g*r_g*(g_d-x_(i,d)
;;;     * Update the particle's position: x_i <- x_i+v_i
;;;     * If (f(x_i) < f(p_i)) do:
;;;       * Update the particle's best known position: p_i <- x_i
;;;       * If (f(p_i) < (f(g)) update the swarm's best known position: g <- p_i
;;;   * Now g holds the best found solution.
;;;   
;;;   - Wikipedia

(defpackage  #:particle-swarm-optimization
  (use #:cl)
  )
