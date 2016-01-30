typedef int NUM;

typedef struct server {
    NUM size, capcity;
} * SERVER;

SERVER newServer ( NUM size, NUM capacity )
{
    SERVER newServ = malloc( sizeof( struct server ) );
    newServer -> size = size;
    newServer -> capacity = capacity;
    return newServer;
}

/*
typedef struct pool {
    NUM * cap_by_row; // Stores the capacity of the pool that comes from each row
} POOL;
*/

typedef ( NUM * ) POOL; // Stores the capacity of the pool that comes from each row

POOL newPool ( NUM rows )
{
    return calloc( sizeof( NUM ) * rows );
}

typedef ( SERVER ) DATACENTER; // must be seen as a matrix that stores the info of the row

DATACENTER newDataCenter ( NUM rows, NUM columns )
{
    return calloc( sizeof( struct server ) * rows * columns );
}
