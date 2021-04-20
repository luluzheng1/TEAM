#include <stdlib.h>
#include <stdio.h>
#include <string.h>
typedef struct list_item_
{
    void *dat;
    struct list_item_ *next;
} list_item;

list_item **get_list()
{
    list_item *test = malloc(sizeof(list_item));
    char *s = "hello_world";
    char *s2 = (char *)malloc(strlen(s) + 1);
    strcpy(s2, s);
    test->dat = s2;
    test->next = NULL;
    list_item **ret = malloc(sizeof(list_item *));
    *ret = test;
    return ret;
}

// list_item **get_list()
// {
//     list_item *test = malloc(sizeof(list_item));
//     int32_t *d = malloc(sizeof(int32_t));
//     *d = 6135;
//     test->dat = d;
//     test->next = NULL;
//     list_item **ret = malloc(sizeof(list_item *));
//     *ret = test;
//     return ret;
// }

list_item **add_list(char *s, list_item **curr)
{
    list_item *node = malloc(sizeof(list_item));
    node->dat = s;
    node->next = NULL;
    while ((*curr)->next != NULL)
    {
        (*curr)->next = (*curr)->next->next;
    }
    (*curr)->next = node;
    return curr;
}
// #ifdef BUILD_TEST
int main()
{
    list_item **l = get_list();
    char *s = (char *)((*l)->dat);
    printf("%s\n", s);
    l = add_list("goodbye world", l);
    s = (char *)((*l)->next->dat);
    printf("%s\n", s);
}
// #endif