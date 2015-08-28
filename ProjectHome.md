Delphi完成端口通讯框架<br>
开发环境：Delphi XE2<br>

在我的机器上做了个IocpHttpServer和Node.js对比的简单测试：<br>
机器配置：<br>
CPU：i5 2500K<br>
内存：16G<br>
两个服务端都只返回字符串“Hello World”<br>
<br>
10000并发的测试：<br>
ab -n 100000 -c 10000 -k <a href='http://127.0.0.1:8000/'>http://127.0.0.1:8000/</a><br>
<br>
IocpHttpServer：<br>
Requests per second:    5529.35 <a href='#/sec.md'>#/sec</a> (mean)<br>
<br>
Node.js：<br>
Requests per second:    411.25 <a href='#/sec.md'>#/sec</a> (mean)<br>
<br>
<br>
100并发的测试：<br>
ab -n 100000 -c 100 -k <a href='http://127.0.0.1:8000/'>http://127.0.0.1:8000/</a><br>
<br>
IocpHttpServer：<br>
Requests per second:    56015.29 <a href='#/sec.md'>#/sec</a> (mean)<br>
<br>
Node.js：<br>
Requests per second:    314.72 <a href='#/sec.md'>#/sec</a> (mean)<br>
很神奇，Node.js并发数少每秒处理请求数反而下降了<br>
<br>
<br>
结论：IOCP真的相当高效！！！！！！！！