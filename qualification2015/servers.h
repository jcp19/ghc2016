typedef struct server {
    int size, capcity;
} * SERVER;

SERVER newServer ( int size, int capacity )
{
    SERVER newServ = malloc (sizeof(struct server));
    newServer -> size = size;
    newServer -> capacity = capacity;
    return newServer;
}

