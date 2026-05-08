#QUICK-SORT:
def partition(a,p,r):
    x=a[r]
    i=p-1
    porownania=0
    for j in range(p,r):
        porownania+=1
        if a[j]<=x:
            i+=1
            a[i],a[j]=a[j],a[i]
    a[i+1],a[r]=a[r],a[i+1]
    return i+1, porownania

def quick_sort(a,p,r):
    porownania=0
    if p<r:
        q,por=partition(a,p,r)
        porownania+=por
        por_lewa=quick_sort(a,p,q-1)
        por_prawa=quick_sort(a,q+1,r)
        porownania+=por_lewa+por_prawa
    return porownania


#HEAP-SORT:
def max_heapify(a,n,i):
    porownania=0
    lewy=2*i+1
    prawy=2*i+2

    if lewy<n:
        porownania+=1
        if a[lewy]>a[i]:
            najw=lewy
        else:
            najw=i
    else:
        najw=i

    if prawy<n:
        porownania+=1
        if a[prawy]>a[najw]:
            najw=prawy

    if najw!=i:
        a[i],a[najw]=a[najw],a[i]
        porownania+=max_heapify(a,n,najw)

    return porownania

def build_max_heap(a):
    porownania=0
    n=len(a)
    
    for i in range(n//2-1,-1,-1):
        porownania+=max_heapify(a,n,i)

    return porownania
    
def heap_sort(a):
    porownania=0
    porownania+=build_max_heap(a)
    n=len(a)

    for i in range(n,1,-1):
        a[0],a[i-1]=a[i-1],a[0]
        porownania+=max_heapify(a,i-1,0)
    return porownania

#INSERTION_SORT:
def insertion_sort(a):
    porownania=0
    n=len(a)

    for i in range(1,n):
        k=a[i]
        j=i-1
        while j>=0:
            porownania+=1
            if a[j]>k:
                a[j+1]=a[j]
                j-=1
            else:
                break
        a[j+1]=k

    return porownania


#BUBBLE_SORT:
def bubble_sort(a):
    porownania=0
    n=len(a)

    for i in range(n-1):
        for j in range(n-1-i):
            porownania+=1
            if a[j]>a[j+1]:
                a[j],a[j+1]=a[j+1],a[j]
    return porownania
    
