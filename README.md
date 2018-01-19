# SalesPredict
Sales Prediction

本项目来自于真实的商业需求，根据国外某电子产品厂家的历史销售数据，库存数据，促销数据，应用统计学习方法对该厂的业务提出改进。

数据：
  1. 按月库存数据（各产品及总量）
  2. 按月销售数据（各产品分地区销量及总量）
  3. 促销数据（参与促销产品及相关促销结果）
  
最终实现功能：
  1. 对未来销售做出预测（时序数据分析）
  2. 提出较优绑定销售产品（关联数据挖掘）
  3. 库存调优（基于销售预测的库存管理建议）

访问地址：
  https://hengtian.shinyapps.io/SalesPOC/


效果图：
  1.库存优化.PNG；
  2.促销预估.PNG；
  3.销量预测.PNG。

技术栈：
  1.R： shiny、ggplot2、 dygraphs；
  2.数据分析： 关联数据挖掘、时间序列分析、回归分析。

模块：
  1.产品分析：上升型产品和下降型产品；
  2.销量：统计分布直方图展示各产品受欢迎度；
  3.预测：根据历史销售数据对未来销量做出预测；
  4.促销分析：对促销组合产品和促销时间（星期X）做出分析，动态给出不同组合及时间预计产生的促销效果；
  5.库存优化：根据预测销量及历史销量，对当期库存数做出预测，优化库存，减少存货。
