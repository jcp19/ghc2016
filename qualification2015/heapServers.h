#include "servers.h"

#define P(i) (i-1)/2
#define L(i) 2*i + 1
#define R(i) 2*i + 2

typedef struct heap{
	SERVER *heap;
	int size;
	int used;
}*HEAP;

void initHeap(HEAP h, int size)
{
	h->heap = malloc(sizeof(struct server)*size);
	h->size = size;
	h->used = 0;
}

void insertServer(HEAP h, SERVER s)
{
	if(h->used == h->size)
	{
		h->heap = realloc(h->heap, sizeof(struct server)*size*2);
		h->size *= 2;
	}

	h->heap[h->used] = s;
	h->used++;
	bubbleUp(h->values, h->used-1);
}

void readServers(HEAP h)
{
	int s, c; //size, capacity
	while(scanf("%d %d", &s, &c))
		insertServer(h, newServer(l, c));
}

SERVER extractServer(HEAP h)
{
	if(h->used > 0)
	{
		SERVER s = h->heap[0];
		h->heap[0] = h->heap[h->used-1];
		h->used--;
		bubbleDown(h->heap, h->used);
		return s;
	}
	else 
		return NULL;
}

void bubbleUp(SERVER *h, int i)
{
	while(i && h[P(i)]->racio < h[i]->racio)
	{
		swap(h, i, P(i));
		i = P(i);
	}
}

void bubbleDown(SERVER *h, int i)
{
	int j = 0, max;
	while(L(j) < i)
	{
		max = h[j]->racio < h[L(j)]->racio ? L(j) : j;
		
		if(R(j) < i)
			max = h[max]->racio < h[R(j)]->racio ? R(j) : max;

		if(max != j)
		{
			swap(h, j, max);
			j = max;
		}
		else break;
	}

}

void swap(SERVER *h, int a, int b)
{
	SERVER aux = h[a];
	h[a] = h[b];
	h[b] = aux;
}
