def split_tim(text, separator):
    res = []
    length = len(text)
    left = right = 0
    while right < length:
        if text[right] == separator:
            res.append(text[left:right])
            right += 1
            left = right
        else:
            right += 1
    res.append(text[left:right])
    return res

print(split_tim("hello,world", ","))
