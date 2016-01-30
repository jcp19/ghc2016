#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "servers.h"
#define FREE NULL

int main ()
{   
    SERVER OCCUPIED = newServer( 0, 0 );
    POOL pool;
    DATACENTER dc;
    int npools, nservers, nrows, nslots, nocuppied, i, x, y;

    scanf( "%d %d %d %d %d", &nrows, &nslots, &nocuppied, &npools, &nservers );
    dc = newDataCenter( nrows, nslots );       
    
    POOL * pools = malloc( npools * sizeof( POOL ) );
    for( i = 0; i < npools; i++ )
       pools[i] = newPool( nrows );
    
    #define POS(i,j) pools[ i * (nslots) + j ]     
    for( i = 0; i < noccupied; i++ ){
       scanf( "%d %d", &x, &y );
       POS( x, y ) = OCCUPIED;
    }
    
    /* now we can insert the servers on the maxheap and pull from it to the dataCenter */
    cria tabela();
    cria_pools();
    le_do_ficheiro();
    ordena_elems_por_crit();
    insere_elems();
    imprimelista();
}
