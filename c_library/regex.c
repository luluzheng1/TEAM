#include <regex.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

/* struct representation of a list node */
typedef struct list_item_
{
    void *dat;
    struct list_item_ *next;
} list_item;

list_item **append(char *s, list_item **head_ref)
{
    list_item *node = malloc(sizeof(list_item));
    char **s_ptr = malloc(sizeof(char *));
    *s_ptr = s;
    node->dat = s_ptr;
    node->next = NULL;
    list_item *last = *head_ref;

    if (*head_ref == NULL)
    {
        *head_ref = node;
        return head_ref;
    }
    while (last->next != NULL)
    {
        last = last->next;
    }
    last->next = node;

    return head_ref;
}

/* prints the content in the list, for debugging purposes */
// void print_list(list_item **head_ref)
// {
//     list_item *temp = *head_ref;

//     if ((*head_ref)->dat == NULL && (*head_ref)->next == NULL)
//     {
//         return;
//     }

//     while (temp != NULL)
//     {
//         printf("%s\n", (*(char **)temp->dat));
//         if (temp->next == NULL)
//         {
//             break;
//         }

//         temp = temp->next;
//     }
// }

int length(list_item **head_ref)
{
    list_item *temp = *head_ref;
    int count = 0;
    if ((*head_ref)->dat == NULL && (*head_ref)->next == NULL)
    {
        return count;
    }

    while (temp != NULL)
    {
        count += 1;
        if (temp->next == NULL)
        {
            break;
        }

        temp = temp->next;
    }
    return count;
}
/*
 * Match string against the extended regular expression in
 * pattern, treating errors as no match.
 *
 * Return 1 for match, 0 for no match.
 */

int match(const char *target, char *pattern)
{
    int status;
    regex_t re;

    if (regcomp(&re, pattern, REG_EXTENDED | REG_NOSUB) != 0)
    {
        return (0); /* Report error. */
    }
    /* re = precompiled pattern
     * string = pattern to be match in regex
     * NULL = information regarding location of the matches
     * 0 = to specify the change in matching behaviour */

    status = regexec(&re, target, (size_t)0, NULL, 0);
    regfree(&re);
    if (status != 0)
    {
        return (0); /* Report error. */
    }
    return (1);
}

void substr(char *str, char *sub, int start, int len)
{
    memcpy(sub, &str[start], len);
    sub[len] = '\0';
}

/* Takes a target string and a regex and returns the 
 * first substring of the string that matches 
 * the regular expression. 
 * 
 * If no match is found, return the empty string.
 */

char *find(char *target, char *regex)
{
    pcre2_code *re;
    PCRE2_SPTR subject = (const unsigned char *)target;
    PCRE2_SPTR pattern = (const unsigned char *)regex;
    int errornumber;
    int rc;

    PCRE2_SIZE erroroffset;
    PCRE2_SIZE *ovector;
    PCRE2_SIZE subject_length;

    pcre2_match_data *match_data;

    subject_length = (PCRE2_SIZE)strlen((char *)subject);

    /* compile the regular expression pattern, and handle
       any errors that are detected. */

    re = pcre2_compile(
        pattern,               /* the pattern */
        PCRE2_ZERO_TERMINATED, /* indicates pattern is zero-terminated */
        0,                     /* default options */
        &errornumber,          /* for error number */
        &erroroffset,          /* for error offset */
        NULL);                 /* use default compile context */

    /* Compilation failed: print the error message and exit. */

    if (re == NULL)
    {
        PCRE2_UCHAR buffer[256];
        pcre2_get_error_message(errornumber, buffer, sizeof(buffer));
        printf("PCRE2 compilation failed at offset %d: %s\n", (int)erroroffset,
               buffer);
        return "";
    }

    match_data = pcre2_match_data_create_from_pattern(re, NULL);

    /* Now run the match. */

    rc = pcre2_match(
        re,             /* the compiled pattern */
        subject,        /* the subject string */
        subject_length, /* the length of the subject */
        0,              /* start at offset 0 in the subject */
        0,              /* default options */
        match_data,     /* block for storing the result */
        NULL);          /* use default match context */

    /* Matching failed: handle error cases */

    if (rc < 0)
    {
        switch (rc)
        {
        case PCRE2_ERROR_NOMATCH:
            /* No Match */
            break;
            /* Handle other special cases if you like */
        default:
            /* Some other match error */
            break;
        }
        pcre2_match_data_free(match_data); /* Release memory used for the match */
        pcre2_code_free(re);               /*   data and the compiled pattern. */
        return "";
    }

    /* Match succeeded. Get a pointer to the output vector, 
     * where string offsets are stored. */

    ovector = pcre2_get_ovector_pointer(match_data);
    // printf("Match succeeded at offset %d\n", (int)ovector[0]);

    /* The output vector wasn't big enough. 
     * This should not happen, because we used 
     * pcre2_match_data_create_from_pattern() above. */

    if (rc == 0)
        printf("ovector was not big enough for all the captured substrings\n");

    /* Show substrings stored in the output vector by number.  */

    PCRE2_SPTR substring_start = subject + ovector[0];
    PCRE2_SIZE substring_length = ovector[1] - ovector[0];
    char *sub = (char *)malloc(sizeof(char) * substring_length);
    substr((char *)substring_start, sub, 0, (int)substring_length);
    return sub;
}

list_item **find_all(char *target, char *regex)
{
    pcre2_code *re;
    PCRE2_SPTR name_table;
    PCRE2_SPTR subject = (const unsigned char *)target;
    PCRE2_SPTR pattern = (const unsigned char *)regex;
    int crlf_is_newline;
    int errornumber;
    int i;
    int rc;
    int utf8;

    uint32_t option_bits;
    uint32_t namecount;
    uint32_t name_entry_size;
    uint32_t newline;

    PCRE2_SIZE erroroffset;
    PCRE2_SIZE *ovector;
    PCRE2_SIZE subject_length;

    pcre2_match_data *match_data;

    subject_length = (PCRE2_SIZE)strlen((char *)subject);
    list_item **ret = malloc(sizeof(list_item *));
    *ret = NULL;
    /* compile the regular expression pattern, and handle
       any errors that are detected. */

    re = pcre2_compile(
        pattern,               /* the pattern */
        PCRE2_ZERO_TERMINATED, /* indicates pattern is zero-terminated */
        0,                     /* default options */
        &errornumber,          /* for error number */
        &erroroffset,          /* for error offset */
        NULL);                 /* use default compile context */

    /* Compilation failed: print the error message and exit. */

    if (re == NULL)
    {
        PCRE2_UCHAR buffer[256];
        pcre2_get_error_message(errornumber, buffer, sizeof(buffer));
        printf("PCRE2 compilation failed at offset %d: %s\n", (int)erroroffset,
               buffer);
        return ret;
    }

    match_data = pcre2_match_data_create_from_pattern(re, NULL);

    /* Now run the match. */

    rc = pcre2_match(
        re,             /* the compiled pattern */
        subject,        /* the subject string */
        subject_length, /* the length of the subject */
        0,              /* start at offset 0 in the subject */
        0,              /* default options */
        match_data,     /* block for storing the result */
        NULL);          /* use default match context */

    /* Matching failed: handle error cases */

    if (rc < 0)
    {
        list_item *list = malloc(sizeof(list_item));
        switch (rc)
        {
        case PCRE2_ERROR_NOMATCH:
            /* no match found, return empty list */
            list->dat = NULL;
            list->next = NULL;
            *ret = list;
            break;
        /*
    Handle other special cases if you like
    */
        default:
            printf("Matching error %d\n", rc);
            break;
        }
        pcre2_match_data_free(match_data); /* Release memory used for the match */
        pcre2_code_free(re);               /*   data and the compiled pattern. */
        return ret;
    }

    /* Match succeeded. Get a pointer to the output vector, where string offsets are
stored. */

    ovector = pcre2_get_ovector_pointer(match_data);
    // printf("Match succeeded at offset %d\n", (int)ovector[0]);

    /* The output vector wasn't big enough. This should not happen, because we used
pcre2_match_data_create_from_pattern() above. */

    if (rc == 0)
        printf("ovector was not big enough for all the captured substrings\n");

    /* Show substrings stored in the output vector by number. Obviously, in a real
application you might want to do things other than print them. */

    PCRE2_SPTR substring_start = subject + ovector[0];
    PCRE2_SIZE substring_length = ovector[1] - ovector[0];
    char *sub = (char *)malloc(sizeof(char) * (substring_length + 1));
    substr((char *)substring_start, sub, 0, (int)substring_length);
    ret = append(sub, ret);
    /* See if there are any named substrings, and if so, show them by name. First
we have to extract the count of named parentheses from the pattern. */

    (void)pcre2_pattern_info(
        re,                   /* the compiled pattern */
        PCRE2_INFO_NAMECOUNT, /* get the number of named substrings */
        &namecount);          /* where to put the answer */

    if (namecount == 0)
    {
    }
    else
    {
        PCRE2_SPTR tabptr;

        /* Before we can access the substrings, we must extract the table for
  translating names to numbers, and the size of each entry in the table. */

        (void)pcre2_pattern_info(
            re,                   /* the compiled pattern */
            PCRE2_INFO_NAMETABLE, /* address of the table */
            &name_table);         /* where to put the answer */

        (void)pcre2_pattern_info(
            re,                       /* the compiled pattern */
            PCRE2_INFO_NAMEENTRYSIZE, /* size of each entry in the table */
            &name_entry_size);        /* where to put the answer */

        /* Now we can scan the table and, for each entry, print the number, the name,
  and the substring itself. In the 8-bit library the number is held in two
  bytes, most significant first. */

        tabptr = name_table;
        for (i = 0; i < namecount; i++)
        {
            int n = (tabptr[0] << 8) | tabptr[1];
            printf("(%d) %*s: %.*s\n", n, name_entry_size - 3, tabptr + 2,
                   (int)(ovector[2 * n + 1] - ovector[2 * n]), subject + ovector[2 * n]);
            tabptr += name_entry_size;
        }
    }

    /* Before running the loop, check for UTF-8 and whether CRLF is a valid newline
sequence. First, find the options with which the regex was compiled and extract
the UTF state. */

    (void)pcre2_pattern_info(re, PCRE2_INFO_ALLOPTIONS, &option_bits);
    utf8 = (option_bits & PCRE2_UTF) != 0;

    /* Now find the newline convention and see whether CRLF is a valid newline
sequence. */

    (void)pcre2_pattern_info(re, PCRE2_INFO_NEWLINE, &newline);
    crlf_is_newline = newline == PCRE2_NEWLINE_ANY ||
                      newline == PCRE2_NEWLINE_CRLF ||
                      newline == PCRE2_NEWLINE_ANYCRLF;

    /* Loop for second and subsequent matches */

    for (;;)
    {
        uint32_t options = 0;                 /* Normally no options */
        PCRE2_SIZE start_offset = ovector[1]; /* Start at end of previous match */

        /* If the previous match was for an empty string, we are finished if we are
  at the end of the subject. Otherwise, arrange to run another match at the
  same point to see if a non-empty match can be found. */

        if (ovector[0] == ovector[1])
        {
            if (ovector[0] == subject_length)
                break;
            options = PCRE2_NOTEMPTY_ATSTART | PCRE2_ANCHORED;
        }
        else
        {
            PCRE2_SIZE startchar = pcre2_get_startchar(match_data);
            if (start_offset <= startchar)
            {
                if (startchar >= subject_length)
                    break;                    /* Reached end of subject.   */
                start_offset = startchar + 1; /* Advance by one character. */
                if (utf8)                     /* If UTF-8, it may be more  */
                {                             /*   than one code unit.     */
                    for (; start_offset < subject_length; start_offset++)
                        if ((subject[start_offset] & 0xc0) != 0x80)
                            break;
                }
            }
        }

        /* Run the next matching operation */

        rc = pcre2_match(
            re,             /* the compiled pattern */
            subject,        /* the subject string */
            subject_length, /* the length of the subject */
            start_offset,   /* starting offset in the subject */
            options,        /* options */
            match_data,     /* block for storing the result */
            NULL);          /* use default match context */

        if (rc == PCRE2_ERROR_NOMATCH)
        {
            if (options == 0)
            {
                break;
            }
            /* All matches found */
            ovector[1] = start_offset + 1;           /* Advance one code unit */
            if (crlf_is_newline &&                   /* If CRLF is a newline & */
                start_offset < subject_length - 1 && /* we are at CRLF, */
                subject[start_offset] == '\r' &&
                subject[start_offset + 1] == '\n')
                ovector[1] += 1;                    /* Advance by one more. */
            else if (utf8)                          /* Otherwise, ensure we */
            {                                       /* advance a whole UTF-8 */
                while (ovector[1] < subject_length) /* character. */
                {
                    if ((subject[ovector[1]] & 0xc0) != 0x80)
                        break;
                    ovector[1] += 1;
                }
            }
            continue; /* Go round the loop again */
        }

        /* Other matching errors are not recoverable. */

        if (rc < 0)
        {
            printf("Matching error %d\n", rc);
            pcre2_match_data_free(match_data);
            pcre2_code_free(re);
            return ret;
        }

        /* Match succeeded */

        // printf("\nMatch succeeded again at offset %d\n", (int)ovector[0]);

        /* The match succeeded, but the output vector wasn't big enough. This
  should not happen. */

        if (rc == 0)
            printf("ovector was not big enough for all the captured substrings\n");

        /* We must guard against patterns such as /(?=.\K)/ that use \K in an
  assertion to set the start of a match later than its end. In this
  demonstration program, we just detect this case and give up. */

        if (ovector[0] > ovector[1])
        {
            printf("\\K was used in an assertion to set the match start after its end.\n"
                   "From end to start the match was: %.*s\n",
                   (int)(ovector[0] - ovector[1]),
                   (char *)(subject + ovector[1]));
            printf("Run abandoned\n");
            pcre2_match_data_free(match_data);
            pcre2_code_free(re);
            return ret;
        }

        /* As before, show substrings stored in the output vector by number, and then
  also any named substrings. */

        PCRE2_SPTR substring_start = subject + ovector[0];
        size_t substring_length = ovector[1] - ovector[0];
        char *sub = (char *)malloc(sizeof(char) * (substring_length + 1));
        substr((char *)substring_start, sub, 0, (int)substring_length);
        ret = append(sub, ret);

        if (namecount == 0)
        {
        }
        else
        {
            PCRE2_SPTR tabptr = name_table;
            for (i = 0; i < namecount; i++)
            {
                int n = (tabptr[0] << 8) | tabptr[1];
                printf("(%d) %*s: %.*s\n", n, name_entry_size - 3, tabptr + 2,
                       (int)(ovector[2 * n + 1] - ovector[2 * n]), subject + ovector[2 * n]);
                tabptr += name_entry_size;
            }
        }
    } /* End of loop to find second and subsequent matches */

    pcre2_match_data_free(match_data);
    pcre2_code_free(re);
    return ret;
}

char *str_replace(char *orig, char *rep, char *with)
{
    char *result;  // the return string
    char *ins;     // the next insert point
    char *tmp;     // varies
    int len_rep;   // length of rep (the string to remove)
    int len_with;  // length of with (the string to replace rep with)
    int len_front; // distance between rep and end of last rep
    int count;     // number of replacements

    // sanity checks and initialization
    if (!orig || !rep)
        return NULL;
    len_rep = strlen(rep);
    if (len_rep == 0)
        return NULL; // empty rep causes infinite loop during count
    if (!with)
        with = "";
    len_with = strlen(with);

    // count the number of replacements needed
    ins = orig;
    for (count = 0; (tmp = strstr(ins, rep)); ++count)
    {
        ins = tmp + len_rep;
    }

    tmp = result = malloc(strlen(orig) + (len_with - len_rep) * count + 1);

    if (!result)
        return orig;

    // only replace the first one
    ins = strstr(orig, rep);
    len_front = ins - orig;
    tmp = strncpy(tmp, orig, len_front) + len_front;
    tmp = strcpy(tmp, with) + len_with;
    orig += len_front + len_rep; // move to next "end of rep"

    strcpy(tmp, orig);
    return result;
}
// You must free the result if result is non-NULL.
char *str_replace_all(char *orig, char *rep, char *with)
{
    char *result;  // the return string
    char *ins;     // the next insert point
    char *tmp;     // varies
    int len_rep;   // length of rep (the string to remove)
    int len_with;  // length of with (the string to replace rep with)
    int len_front; // distance between rep and end of last rep
    int count;     // number of replacements

    // sanity checks and initialization
    if (!orig || !rep)
        return NULL;
    len_rep = strlen(rep);
    if (len_rep == 0)
        return NULL; // empty rep causes infinite loop during count
    if (!with)
        with = "";
    len_with = strlen(with);

    // count the number of replacements needed
    ins = orig;
    for (count = 0; (tmp = strstr(ins, rep)); ++count)
    {
        ins = tmp + len_rep;
    }

    tmp = result = malloc(strlen(orig) + (len_with - len_rep) * count + 1);

    if (!result)
        return orig;

    // first time through the loop, all the variable are set correctly
    // from here on,
    //    tmp points to the end of the result string
    //    ins points to the next occurrence of rep in orig
    //    orig points to the remainder of orig after "end of rep"
    while (count--)
    {
        ins = strstr(orig, rep);
        len_front = ins - orig;
        tmp = strncpy(tmp, orig, len_front) + len_front;
        tmp = strcpy(tmp, with) + len_with;
        orig += len_front + len_rep; // move to next "end of rep"
    }
    strcpy(tmp, orig);
    return result;
}

char *replace(char *target, char *regex, char *replc, int count)
{
    char *result = malloc(sizeof(char) * strlen(target));
    strcpy(result, target);
    while (count--)
    {
        char *sub = find(result, regex);
        if (strcmp(sub, "") == 0)
        {
            break;
        }
        result = str_replace(result, sub, replc);
    }
    return result;
}

char *replace_all(char *target, char *regex, char *replc)
{
    char *result = malloc(sizeof(char) * strlen(target));
    strcpy(result, target);
    for (;;)
    {
        char *sub = find(result, regex);
        if (strcmp(sub, "") == 0)
        {
            break;
        }
        result = str_replace(result, sub, replc);
    }
    return result;
}

#ifdef BUILD_TEST
void test_replace_all()
{
    char *s = replace_all("google ggle goooogle", "go*gle", "replaced");
    assert(strcmp(s, "replaced replaced replaced") == 0);
    s = replace_all("This dog is grey and this cat is gray.", "gr(a|e)y", "brown");
    assert(strcmp(s, "This dog is brown and this cat is brown.") == 0);
    s = replace_all("Hello hello hello", "hello", "bye");
    assert(strcmp(s, "Hello bye bye") == 0);
}
void test_find_all()
{
    list_item **list = find_all("hello hello hello", "hello");
    assert(length(list) == 3);
    list_item **list2 = find_all("google ggle goooogle", "go*gle");
    assert(length(list2) == 3);
    list_item **list3 = find_all("gray", "gr(a|e)y");
    assert(length(list3) == 1);
    list_item **list4 = find_all("2+2 3*3 4-4 5+5 6*6", "\\d+[\\+-x\\*]\\d+");
    assert(length(list4) == 5);
    list_item **list5 = find_all("This is a dog", "^dog");
    assert(length(list5) == 0);
}
void test_find()
{
    char *s = find("ggle google", "go*gle");
    assert(strcmp(s, "ggle") == 0);
    s = find("goooogle", "go*gle");
    assert(strcmp(s, "goooogle") == 0);
    s = find("grey", "gr(a|e)y");
    assert(strcmp(s, "grey") == 0);
    s = find("There goes the bat", "[b-chm-pP]at|ot");
    assert(strcmp(s, "bat") == 0);
    s = find("a", "\\w");
    assert(strcmp(s, "a") == 0);
    s = find("2+2", "\\d+[\\+-x\\*]\\d+");
    assert(strcmp(s, "2+2") == 0);
    s = find("x+y", "\\w+[\\+-x\\*]\\w+");
    assert(strcmp(s, "x+y") == 0);
    s = find("?+?", "\\W+[\\+-x\\*]\\W+");
    assert(strcmp(s, "?+?") == 0);
    // Begins with dog
    s = find("dog collar", "^dog");
    assert(strcmp(s, "dog") == 0);
    // No match
    s = find("This is a dog", "^dog");
    assert(strcmp(s, "") == 0);
    // Ends with dog
    s = find("hot dog", "dog$");
    assert(strcmp(s, "dog") == 0);
    // Just dog
    s = find("dog", "^dog$");
    assert(strcmp(s, "dog") == 0);
    // Time in 24 hr format
    s = find("12:03", "^([01]?[0-9]|2[0-3]):[0-5][0-9]$");
    assert(strcmp(s, "12:03") == 0);
    // No match
    s = find("z", "z{3,6}");
    assert(strcmp(s, "") == 0);
}
void test_match()
{
    int i = match("ggle google", "go*gle");
    assert(i == 1);
    i = match("goooogle", "go*gle");
    assert(i == 1);
    i = match("grey", "gr(a|e)y");
    assert(i == 1);
    i = match("There goes the bat", "[b-chm-pP]at|ot");
    assert(i == 1);
    i = match("a", "\\w");
    assert(i == 1);
    i = match("2+2", "\\d+[\\+-x\\*]\\d+");
    assert(i == 1);
    i = match("x+y", "\\w+[\\+-x\\*]\\w+");
    assert(i == 1);
    i = match("?+?", "\\W+[\\+-x\\*]\\W+");
    assert(i == 1);
    // Begins with dog
    i = match("dog collar", "^dog");
    assert(i == 1);
    // No match
    i = match("This is a dog", "^dog");
    assert(i == 0);
    // Ends with dog
    i = match("hot dog", "dog$");
    assert(i == 1);
    // Just dog
    i = match("dog", "^dog$");
    assert(i == 1);
    // Time in 24 hr format
    i = match("12:03", "^([01]?[0-9]|2[0-3]):[0-5][0-9]$");
    assert(i == 1);
    // No match
    i = match("z", "z{3,6}");
    assert(i == 0);
}
void test_replace()
{
    char *s = replace("ggle google", "go*gle", "replaced", 1);
    assert(strcmp(s, "replaced google") == 0);
    s = replace("ggle google", "go*gle", "replaced", 5);
    assert(strcmp(s, "replaced replaced") == 0);
    s = replace("ggle google", "go*gle", "replaced", 0);
    assert(strcmp(s, "ggle google") == 0);
    s = replace("google google google", "goo", "foo", 3);
    assert(strcmp(s, "foogle foogle foogle") == 0);
}
int main()
{
    test_replace_all();
    test_find();
    test_match();
    test_replace();
    test_find_all();
    return 0;
}
#endif