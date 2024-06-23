-- 清屏
system clear

-- 查询学生的个数
select count(height) from students;
注意点: 聚合函数不会对空值进行统计
一般如果要是指定列名，那么就是主键字段
select count(id) from students;
通用的写法
select count(*) from students;


-- 查询女生的编号最大值

select max(id) from students where gender = '女';

-- 查询未删除的学生最小编号
select min(id) from students where is_del = 0;


-- 查询男生的总身高
select sum(height) from students where gender='男';



-- 求男生的平均身高
select sum(height) / count(*) from students where gender='男';
select avg(height) from students where gender = '男';
select avg(ifnull(height, 0)) from students where gender = '男';
注意点: 聚合函数不会对空值进行统计
ifnull函数判断指定的字段是否是空值，如果是空值使用默认值0

-- 查询性别的种类
select distinct gender from students;
select gender from students group by gender;



-- 根据name和gender字段进行分组, 查看name和gender的分组信息
select gender,name from students group by gender,name;


-- 根据gender字段进行分组， 查询每个分组的姓名信息
select gender,group_concat(name) from students group by gender;
group_concat：统计每个分组指定字段的信息集合，信息之间使用逗号进行分割


-- 统计不同性别的平均年龄
select gender,avg(age) from students group by gender;


-- 统计不同性别的人的个数
select gender,count(*) from students group by gender;



-- 根据gender字段进行分组，统计分组条数大于2的
select gender,count(*) from students group by gender having count(*) > 2;

-- 对分组数据进行过滤使用having


-- 根据gender字段进行分组，汇总总人数
select gender,count(*) from students group by gender with rollup;

-- 根据gender字段进行分组，汇总所有人的年龄
select gender,group_concat(age) from students group by gender with rollup;



-- 使用内连接查询学生表与班级表
select s.name, c.name from students s inner join classes c on s.c_id = c.id;




-- 使用左连接查询学生表与班级表
select * from students s left join classes c on s.c_id = c.id;
左连接查询，根据左表查询右表，如果右表数据不存在使用null填充
left左边是左表，left右边是右表


-- 使用右连接查询学生表与班级表
select * from students s right join classes c on s.c_id = c.id;
右连接查询，根据右表查询左表，如果左表数据不存在使用null填充
right左边是左表，right右边是右表

-- 使用自连接查询省份和城市信息
select c.id, c.title, c.pid, p.title from areas c inner join areas p on c.pid = p.id where p.title = '河北省';



-- 查询大于平均年龄的学生
select * from students where age > (select avg(age) from students);



-- 查询学生在班的所有班级名字
select * from classes where id in (select c_id from students where c_id is not null);



-- 查找年龄最大,身高最高的学生

 select * from students where age = (select max(age) from students) and height = (select max(height) from students);

简写:

select * from students where (age, height) = (select max(age), max(height) from students);


子查询是一个完整的查询语句，子查询的执行顺序，先执行子查询然后主查询根据子查询的结果再执行

-- 为学生版的cls_id字段添加外键约束

-- 创建学校表


-- 创建老师表添加学校外键

-- 删除外键



