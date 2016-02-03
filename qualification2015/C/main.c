#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "servers.h"
#include "heapServers.h"
#define FREE NULL

int main ()
{   
    SERVER OCCUPIED = newServer( 0, 0 );
    POOL pool;
    DATACENTER dc;
	HEAP heap;
    int npools, nservers, nrows, nslots, noccupied, i, x, y;

    scanf( "%d %d %d %d %d", &nrows, &nslots, &noccupied, &npools, &nservers );
    dc = newDataCenter( nrows, nslots );       
    
    POOL * pools = malloc( npools * sizeof( POOL ) );
    for( i = 0; i < npools; i++ )
       pools[i] = newPool( nrows );
    
    #define POS(i,j) pools[ i * (nslots) + j ]     
    for( i = 0; i < noccupied; i++ ){
       scanf( "%d %d", &x, &y );
       POS(x, y) = OCCUPIED;
    }
	
	//reads servers directly into the heap
	initHeap(heap, nservers);
	readServers(heap);


    /* now we can insert the servers on the maxheap and pull from it to the dataCenter */
   /*
    cria tabela();
    cria_pools();
    le_do_ficheiro();
    ordena_elems_por_crit();
    insere_elems();
    imprimelista();
   */
}
