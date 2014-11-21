-define(green_data_template(Num, Seq),
    {[
        {<<"observation">>,
            {[
                {<<"color">>, <<"green">>},
                {<<"numbers">>, Num}
            ]}},
        {<<"sequence">>, Seq}
    ]}
).

-define(red_data_template(Seq),
    {[
        {<<"observation">>,
            {[{<<"color">>, <<"red">>}]}},
        {<<"sequence">>, Seq}]}
).

-define(num0, <<"1110111">>).
-define(num1, <<"0010010">>).
-define(num2, <<"1011101">>).
-define(num3, <<"1011011">>).
-define(num4, <<"0111010">>).
-define(num5, <<"1101011">>).
-define(num6, <<"1101111">>).
-define(num7, <<"1010010">>).
-define(num8, <<"1111111">>).
-define(num9, <<"1111011">>).
