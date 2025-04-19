#<Let: v55492 
  #<Let: _ 
    #<If: 
      #<Prim: < (
        #<Prim: + (#<GlobalValue: free_ptr> #<Int: 24>)>
        #<GlobalValue: fromspace_end>)>
      #<Void:>
      (collect 24)>
    #<Let: tmp55495 (allocate 2 (Vector Integer Integer))
      #<Let: tmp55493 #<Int: 20>
        #<Let: tmp55494 #<Int: 22>
          #<Let: _ #<Prim: vector-set! (#<Var: tmp55495> #<Int: 0> #<Var: tmp55494>)>
            #<Let: _ #<Prim: vector-set! (#<Var: tmp55495> #<Int: 1> #<Var: tmp55493>)>
              #<Var: tmp55495>>>>>>>
  #<Prim: + (#<Prim: vector-ref (#<Var: v55492> #<Int: 0>)> #<Prim: vector-ref (#<Var: v55492> #<Int: 1>)>)>>>

; err
#<Let: v55492
  #<Let: _
    #<If:
      #<Let: tmp55497
        #<Let: tmp55496 #<GlobalValue: free_ptr>
          #<Prim: + (#<Var: tmp55496> #<Int: 24>)>>
        #<Let: tmp55498 #<GlobalValue: fromspace_end>
          #<Prim: < (#<Var: tmp55497> #<Var: tmp55498>)>>>
      #<Void:>
      (collect 24)>
    #<Let: tmp55495 (allocate 2 (Vector Integer Integer))
      #<Let: tmp55493 #<Int: 20>
        #<Let: tmp55494 #<Int: 22>
          #<Let: _ #<Prim: vector-set! (#<Var: tmp55495> #<Int: 0> #<Var: tmp55494>)>
            #<Let: _ #<Prim: vector-set! (#<Var: tmp55495> #<Int: 1> #<Var: tmp55493>)>
              #<Var: tmp55495>>>>>>>
  #<Let: tmp55499 #<Prim: vector-ref (#<Var: v55492> #<Int: 0>)>
    #<Let: tmp55500 #<Prim: vector-ref (#<Var: v55492> #<Int: 1>)>
      #<Prim: + (#<Var: tmp55499> #<Var: tmp55500>)>>>>>

; non err
#<Let: v55492
  #<Let: _
    #<If:
      #<Let: tmp55498 #<GlobalValue: fromspace_end>
        #<Let: tmp55497
          #<Let: tmp55496 #<GlobalValue: free_ptr>
            #<Prim: + (#<Var: tmp55496> #<Int: 24>)>>
          #<Prim: < (#<Var: tmp55497> #<Var: tmp55498>)>>>
      #<Void:>
      (collect 24)>
    #<Let: tmp55495 (allocate 2 (Vector Integer Integer))
      #<Let: tmp55493 #<Int: 20>
        #<Let: tmp55494 #<Int: 22>
          #<Let: _ #<Prim: vector-set! (#<Var: tmp55495> #<Int: 0> #<Var: tmp55494>)>
            #<Let: _ #<Prim: vector-set! (#<Var: tmp55495> #<Int: 1> #<Var: tmp55493>)>
              #<Var: tmp55495>>>>>>>
  #<Let: tmp55500 #<Prim: vector-ref (#<Var: v55492> #<Int: 1>)>
    #<Let: tmp55499 #<Prim: vector-ref (#<Var: v55492> #<Int: 0>)>
      #<Prim: + (#<Var: tmp55499> #<Var: tmp55500>)>>>>>

#<Program: #(struct:Ordl #<procedure:symbol-compare> #(struct:Empty)) #<Let: v55492 #<Let: _ #<If: #<Prim: < (#<Prim: + (#<GlobalValue: free_ptr> #<Int: 24>)> #<GlobalValue: fromspace_end>)> #<Void:> (collect 24)> #<Let: tmp55495 (allocate 2 (Vector Integer Integer)) #<Let: tmp55493 #<Int: 20> #<Let: tmp55494 #<Int: 22> #<Let: _ #<Prim: vector-set! (#<Var: tmp55495> #<Int: 0> #<Var: tmp55494>)> #<Let: _ #<Prim: vector-set! (#<Var: tmp55495> #<Int: 1> #<Var: tmp55493>)> #<Var: tmp55495>>>>>>> #<Prim: + (#<Prim: vector-ref (#<Var: v55492> #<Int: 0>)> #<Prim: vector-ref (#<Var: v55492> #<Int: 1>)>)>>>
#<Program: #(struct:Ordl #<procedure:symbol-compare> #(struct:Empty)) #<Let: v55492 #<Let: _ #<If: #<Let: tmp55498 #<GlobalValue: fromspace_end> #<Let: tmp55497 #<Let: tmp55496 #<GlobalValue: free_ptr> #<Prim: + (#<Var: tmp55496> #<Int: 24>)>> #<Prim: < (#<Var: tmp55497> #<Var: tmp55498>)>>> #<Void:> (collect 24)> #<Let: tmp55495 (allocate 2 (Vector Integer Integer)) #<Let: tmp55493 #<Int: 20> #<Let: tmp55494 #<Int: 22> #<Let: _ #<Prim: vector-set! (#<Var: tmp55495> #<Int: 0> #<Var: tmp55494>)> #<Let: _ #<Prim: vector-set! (#<Var: tmp55495> #<Int: 1> #<Var: tmp55493>)> #<Var: tmp55495>>>>>>> #<Let: tmp55500 #<Prim: vector-ref (#<Var: v55492> #<Int: 1>)> #<Let: tmp55499 #<Prim: vector-ref (#<Var: v55492> #<Int: 0>)> #<Prim: + (#<Var: tmp55499> #<Var: tmp55500>)>>>>>