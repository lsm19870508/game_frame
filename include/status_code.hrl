%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 三月 2016 14:32
%%%-------------------------------------------------------------------
-author("Administrator").
-ifndef(status_code).
-define(status_code, true).

-define(STATUS_ERROR_1, 1). % 操作成功
-define(STATUS_OK, 0). % 操作成功
-define(STATUS_INVALID_REQUEST, -1). % JSON无法解析
-define(STATUS_UNKNOWN_CODE, -2). % 未知请求
-define(STATUS_SRQ_MISSING, -3). % 未提供srq
-define(STATUS_PLAT_ID_MISSING, -4). % 未提供plat_id
-define(STATUS_VERSION_MISSING, -5). % 未提供version
-define(STATUS_TIMESTAMP_MISSING, -6). % 未提供timestamp
-define(STATUS_ACCOUNTID_MISSING, -7). % 未提供账户id
-define(STATUS_ACCTOKEN_MISSING, -8). % 未提供账户token
-define(STATUS_CLEINTIP_MISSING, -9). % 解析客户端ip地址错误
-define(STATUS_SYSTEM_ERROR, -10). % 系统异常，请联系服务端
-define(STATUS_CHANNEL_MISSION, -11). % 未提供渠道，手Q或者微信


-define(STATUS_CLOSE_ACCOUNT, -33). % 账户被封
-define(STATUS_ACCOUNT_OFF, -34). % 账户掉线了
-define(STATUS_SRQ_REPEAT, -35). % 重复操作
-define(STATUS_ACCOUNT_NULL, -36). % 账户数据异常
-define(STATUS_SAVE_ERROR, -37). % 数据库保存失败


-define(STATUS_SERVER_STOP, -41). % 停服维护
-define(STATUS_NOT_ACTIVATION, -42). % 未激活
-define(STATUS_CLOSE_ACCOUNT_FOR_EVER, -43). % 永久封禁


-define(STATUS_PARA_MISSING, -50). % 参数不全

-define(STATUS_GAME_SYSTEM_ERROR, -51). % 逻辑错误

-define(STATUS_PARA_ILLEGAL, -53). % 非法参数

-define(STATUS_OTHER_CLIENT_ONLINE, -69). % 另一台客户端登陆 注释：此错误码前端特殊处理，如有冲突，无修改此错误码

-define(STATUS_REG_LIMIT, -75). % 注册上限
-define(STATUS_ACCOUNT_TOKEN_EXPIRE, -76). % accountToken过期

%%用这个宏来制造统一的错误码
-define(MAKE_ERROR_CODE(Cmd,RetCode),Cmd * 100 + RetCode).
%%下面是计算后的统一错误码
-define(ACCOUNT_REGISTER_DUPLICATE,6000101).%%重名
-define(ACCOUNT_REGISTER_OUTOFLINE,6000102).%%不符合命名规范
-define(ACCOUNT_LOGIN_NONEXISTENT,6000201).%%账号不存在
-define(ACCOUNT_LOGIN_WRONGPASS,6000202).%%密码错误
-define(ACCOUNT_COMPLETE_ALREADLY_EXIST,6000401).%%用户名已存在
-define(ACCOUNT_COMPLETE_ALREADLY_COMPLETE,6000402).%%该用户已补全
-define(ACCOUNT_COMPLETE_ALREADLY_OUTOFLINE,6000403).%%不符合命名规范


-endif.