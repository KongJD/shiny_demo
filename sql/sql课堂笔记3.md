-- 查询类型cate_name为 '超级本' 的商品名称、价格
select name, price from goods where cate_name = '超级本';

-- 显示商品的分类
select distinct cate_name from goods;
select cate_name from goods group by cate_name;


-- 求所有电脑产品的平均价格,并且保留两位小数
select round(avg(price),2) from goods;


-- 显示每种商品的平均价格
select cate_name, avg(price) from goods group by cate_name;

select cate_name, round(avg(price),2) from goods group by cate_name;

-- 查询每种类型的商品中 最贵、最便宜、平均价、数量
select cate_name, max(price), min(price), avg(price), count(*) from goods group by cate_name


-- 查询所有价格大于平均价格的商品，并且按价格降序排序
select * from goods where price > (select avg(price) from goods) order by price desc;



-- 创建商品分类表
create table goods_cates(
    -> id int unsigned not null primary key auto_increment,
    -> name varchar(50) not null);

 
-- 查询goods表中商品的分类信息
 select cate_name from goods group by cate_name;


-- 将查询结果插入到good_cates表中
 insert into goods_cates(name) select cate_name from goods group by cate_name;



-- 添加移动设备分类信息
insert into goods_cates(name) values('移动设备');



-- 查看goods表中的商品分类名称对应的商品分类id

 select * from goods g inner join goods_cates gs on g.cate_name = gs.name;

-- 将goods表中的分类名称更改成商品分类表中对应的分类id，连接更新表中的某个字段

 update goods g inner join goods_cates gs on g.cate_name = gs.name set g.cate_name = gs.id;




-- 查询品牌信息
select brand_name from goods group by brand_name;


-- 通过create table ...select来创建商品品牌表并且同时插入数据
create table goods_brands( id int unsigned not null primary key auto_increment, name varchar(50) not null) select brand_name  as name from goods group by brand_name;

 
-- 插入双飞燕品牌
 insert into goods_brands(name) values('双飞燕'); 

-- 查看goods表中的商品品牌对应的商品品牌id
select * from goods g inner join goods_brands gs on g.brand_name = gs.name;


-- 将goods表中的品牌更改成品牌表中对应的品牌id，连接更新表中的某个字段
 update goods g inner join goods_brands gs on g.brand_name = gs.name set g.brand_name = gs.id;


-- 通过alter table语句修改表结构,把cate_name改成cate_id，把brand_name改成brand_id
 alter table goods change cate_name cate_id int not null, change brand_name brand_id int not null;

