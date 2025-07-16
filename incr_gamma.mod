set A;                     
set NODES;                 

param T;                   
param stage{NODES};        
param parent{NODES} symbolic;       
param prob{NODES};         

param mu{A, NODES};        
param sigma2{A, A};        

param gamma {0..T} > 0;           
param w0 >= 0;              # ricchezza iniziale

var x{A, NODES} >= 0;       
var w{NODES} >= 0;       

#var var{NODES} >= 0;  # varianza portafoglio in ogni nodo   


#CARA utility function
#maximize Expected_Utility:
 #   sum {n in NODES} prob[n] * ( - exp(-gamma[stage[n]] * w[n]) );
    
# --- Funzione di utilità CRRA ---
maximize Expected_Utility:
    sum {n in NODES} prob[n] * (( w[n]^(1 - gamma[stage[n]])-1) / (1 - gamma[stage[n]]) );
    

subject to Initial_Wealth:
    w["n0"] = w0;

subject to Initial_Budget:
    sum {a in A} x[a,"n0"] = w0;

subject to Budget {n in NODES: n != "n0"}:
    sum {a in A} x[a,n] = w[n];

subject to Wealth_Update {n in NODES: n != "n0"}:
    w[n] = sum {a in A} x[a,parent[n]] * (1 + mu[a,n]);
    
subject to MaxAlloc {a in A, n in NODES}:
    x[a,n] <= 0.8 * w[n];
    
#subject to Variance_Definition {n in NODES}:
    #var[n] = sum {a1 in A, a2 in A} x[a1,n] * sigma2[a1,a2] * x[a2,n];
    
 
