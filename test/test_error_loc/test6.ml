let _ = p"\

%s %us \t

{option int}

   {list (array plop)}
                ^
                |
                +----+
                     |
It should fail here -+

"
