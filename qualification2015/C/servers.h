#include <stdlib.h>

typedef int NUM;

typedef struct server {
    NUM size;
    NUM capacity;
    NUM id;
    float ratio;
   
} * SERVER;

SERVER newServer ( NUM size, NUM capacity )
{
    SERVER newServer = (SERVER) malloc( sizeof( struct server ) );
    newServer -> size = size;
    newServer -> capacity = capacity;
    return newServer;
}

/*
typedef struct pool {
    NUM * cap_by_row; // Stores the capacity of the pool that comes from each row
} POOL;
*/

typedef NUM * POOL; // Stores the capacity of the pool that comes from each row

POOL newPool ( NUM rows )
{
    return calloc( rows, sizeof( NUM ) );
}

typedef SERVER * DATACENTER; // must be seen as a matrix that stores the info of the row

DATACENTER newDataCenter ( NUM rows, NUM columns )
{
    return calloc( rows * columns, sizeof(SERVER) );
}
