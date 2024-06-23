--登录数据库
mysql -uroot -p

--显示当前时间
select now();

--登出(退出)数据库
exit/quit/ctr+d

--查看所有数据库
show databases;

--创建数据库
create database python41 charset=utf8;
--使用数据库
use python41;
--查看当前使用的数据库
select database();
--删除数据库-慎重
drop database python41;
--查看当前数据库中所有表
show tables;
--创建表
create table students(id int unsigned primary key auto_increment not null, name varchar(10) not null, age tinyint default 0, gender enum("男", "女") default "男");

--修改表-添加birthday字段
alter table students add birthday datetime not null;


--修改表-修改字段类型
alter table students modify birthday date;


--修改表-修改字段名和字段类型
alter table students change birthday birth datetime not null;



--修改表-删除birthday字段
 alter table students drop birth;


--查看表结构
desc students;

--查看创表SQL语句
show create table students;

--查看创库SQL语句
show create database python41;
--删除表
drop table students;

--查询所有列数据
select * from students;

--查询指定列数据
select name, age from students;
--添加数据--全列插入
insert into students values(0, '张三', 18, default);
主键列表插入数据的时候可以指定: 0、default、null
这里的default表示使用该字段默认值


--添加数据--部分列插入
insert into students(name, age) values('郭靖',30);

--添加数据--全列多行插入
insert into students values(0, '黄蓉', 28, '女'),(0,'黄老邪',50,default); 

--添加数据--部分列多行插入
insert into students(name, age) values('杨过', 20),('周伯通',55);

--修改数据
update students set age=18, gender = '女' where id = 3;

--删除数据
delete from students where id = 8;

--删除数据可以使用逻辑删除，添加一个标识字段
alter table students add is_del tinyint default 0;
这里删除数据其实修改标识字段
update students set is_del = 1 where id = 7;

--as关键字，用户给表的字段和表设置别名
select name as n, age as a from students as s;
提示: as 关键字可以省略，也表示设置别名


--distinct关键字， 用于去除重复的数据行
select distinct age, gender from students;


--查询编号大于3的学生
 select * from students where id > 3;


--查询编号不大于4的学生
select * from students where id <= 4;


--查询姓名不是“黄蓉”的学生
select * from students where name <> '黄蓉';
select * from students where name != '黄蓉';

--查询没被删除的学生
select * from students where is_del = 0;


--查询编号大于3的女同学
select * from students where id > 3 and gender = '女';

--查询编号小于4或没被删除的学生
select * from students where id < 4 or is_del = 0;

--查询年龄不在10岁到15岁之间的学生
select * from students where not (age >= 10 and age <= 15);

--查询姓黄的学生
select * from students where name like '黄%';
--查询姓黄并且“名”是一个字的学生
select * from students where name like '黄_';
select * from students where name like '黄__';
%: 表示任意多个字符
_: 表示任意一个字符


--查询姓黄或叫靖的学生
select * from students where name like '黄%' or name like '%靖';

--查询编号为3至8的学生
select * from students where id >=3 and id <= 8;
select * from students where id between 3 and 8;

--查询编号不是3至8的男生
select * from students where not (id between 3 and 8) and gender='男';

--查询编号是3、5、7的学生
select * from students where id in (3, 5, 7);

--查询编号不是3、5、7的学生
select * from students where id not in (3, 5, 7);


--查询没有填写身高的学生
select * from students where height is null;

--查询填写身高的学生
select * from students where height is  not null;

--查询未删除男生信息，按学号降序
select * from students where is_del = 0 and gender = '男' order by id desc;

--显示所有的学生信息，先按照年龄从大-->小排序，当年龄相同时 按照身高从高-->矮排序
select * from students order by age desc, height desc;
默认是asc 不用指定。


--查询前3行男生信息
select * from students where gender='男' limit 0, 3;
简写方式，第一个参数是开始行索引，默认是0可以不指定， 第二个参数是查询条数
select * from students where gender='男' limit 3;


--查询学生表，获取第n页数据的SQL语句
select * from students limit (n-1) * m, m;






















