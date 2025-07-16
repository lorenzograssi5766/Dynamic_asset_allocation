set A;                     
set NODES;                 

param T;                   
param stage{NODES};        
param parent{NODES} symbolic;       
param prob{NODES};         

param mu{A, NODES};        
param sigma2{A, A};        

param gamma {0..T} > 0;           
param w0 >= 0;              

var x{A, NODES} >= 0;       
var w{NODES} >= 0;       

# --- CRRA utility function ---  
maximize Expected_Utility:
    sum {n in NODES} prob[n] * (
        if gamma[stage[n]] = 1 then
            log(w[n])
        else
            (w[n]^(1 - gamma[stage[n]]) - 1) / (1 - gamma[stage[n]])
    );
    

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
    
 
