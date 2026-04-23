# L-p-tr-nh-ph-n-t-ch-d-li-u

![Uploading image.png…]()


Kịch Bản Thuyết Trình: Xây Dựng Hệ Thống Định Giá & Tuyển Trạch Cầu Thủ Với R & XGBoost
Người trình bày: Nguyễn Hồng Phi - NEU
Thời lượng: ~10 - 12 phút

MỞ ĐẦU (1.5 Phút) - Đặt Vấn Đề Từ Góc Nhìn Dữ Liệu
"Xin chào thầy cô và các bạn, mình là Nguyễn Hồng Phi.

Thị trường chuyển nhượng bóng đá ngày nay không còn là nơi chỉ dành cho những cuộc đàm phán trên bàn giấy, mà đã trở thành một cuộc chạy đua về công nghệ lõi và phân tích dữ liệu. Tuy nhiên, bài toán định giá cầu thủ luôn là một 'hộp đen'. Tại sao một cầu thủ này giá 10 triệu bảng, trong khi người kia với thông số tương đương lại giá 50 triệu bảng?

Hôm nay, mình sẽ trình bày một giải pháp bóc tách cái 'hộp đen' đó. Mình đã xây dựng một hệ thống End-to-End bằng ngôn ngữ R, sử dụng thư viện tidyverse để xử lý dữ liệu và thuật toán học máy xgboost để dự đoán giá trị thực. Hệ thống này không chỉ dừng lại ở việc đưa ra một con số tiền tệ, mà còn tích hợp một 'Scouting Layer' - lớp tự động hóa việc tìm kiếm các Wonderkids (Tài năng trẻ) hay Hidden Gems (Ngọc thô) dựa trên thuật toán chấm điểm đa chiều.

Bây giờ, chúng ta sẽ đi sâu vào kiến trúc hệ thống, bắt đầu từ nền tảng quan trọng nhất: Dữ liệu."

PHẦN 1: FEATURE ENGINEERING - "DẠY" AI HIỂU BÓNG ĐÁ (3 Phút)
"Mọi mô hình Machine Learning đều tuân theo quy tắc: 'Garbage in, Garbage out'. Nếu đưa số liệu thô vào, mô hình sẽ học sai. Trong Phase 1 của dự án, mình đã thực hiện Feature Engineering rất mạnh tay.

Thứ nhất: Xử lý Missing Values và Outliers.
Dữ liệu bóng đá thực tế rất nhiễu. Mình sử dụng hàm replace_na để quy chuẩn các chỉ số vắng mặt về 0, và điền giá trị Median cho chiều cao. Nhưng quan trọng hơn, mình tự viết một hàm cap_quantile để gọt bỏ các giá trị ngoại lai (outliers) ở mốc 1% và 99%. Điều này giúp mô hình không bị 'ảo tưởng' bởi một cầu thủ vô danh vô tình ghi 1 bàn sau 10 phút thi đấu khiến hiệu suất P90 cao đột biến.

Thứ hai: Xử lý sự bất đối xứng của dữ liệu (Skewness).
Các bạn để ý trong code, mình áp dụng hàm log1p() (Logarit cơ số tự nhiên cộng 1) cho hầu hết các biến số chuyên môn như goalsP90, keyPassP90, hay tackleP90. Tại sao lại là log1p? Vì thống kê bóng đá tuân theo phân phối lệch phải (Right-skewed) – đa số cầu thủ ghi ít bàn, chỉ một vài siêu sao ghi rất nhiều bàn. Việc chuyển sang Log-scale giúp dữ liệu phân bố gần với đường cong chuẩn (Normal Distribution) hơn, giúp XGBoost bắt được tín hiệu tốt hơn. Biến mục tiêu cũng được chuyển thành log_marketvalue.

Thứ ba: Các chỉ số tổng hợp (Composite Metrics).
Mình không để AI tự phân tích các con số rời rạc. Mình đã thiết kế các chỉ số đại diện cho tư duy chiến thuật:

Ví dụ biến creativity_index (Chỉ số sáng tạo): Mình không chỉ cộng ngang, mà gán trọng số: xA + 0.5 * keyPass + 0.7 * BigChanceCreated. Một cơ hội lớn rõ ràng có sức nặng hơn một đường chuyền key-pass thông thường.

Biến age_c và age_sq: Mình xác định peak_age (tuổi chín muồi) theo vị trí: Tiền đạo là 27, Thủ môn là 29. Cầu thủ cách độ tuổi đỉnh cao càng xa thì giá trị biến thiên càng lớn theo hàm bậc 2 (age_sq). Đây là cách mình lượng hóa khái niệm 'Đường cong sự nghiệp' vào toán học."

PHẦN 2: KIẾN TRÚC MÔ HÌNH XGBOOST & CALIBRATION (3 Phút)
"Bước sang Phase 2, việc định giá.

Điểm đặc biệt trong thiết kế này là mình không dùng một mô hình duy nhất. Tiêu chí đánh giá một tiền đạo hoàn toàn khác một trung vệ. Mình đã xây dựng list feature_map riêng rẽ cho 4 vị trí: F, M, D, G. Sau đó dùng hàm sparse.model.matrix để chuyển đổi dữ liệu categorical sang ma trận thưa, tối ưu bộ nhớ.

Về mô hình cốt lõi, hàm train_xgb_position chứa những thiết lập Hyperparameters mà mình đã tinh chỉnh (tuning):

Mình sử dụng thuật toán XGBoost (Extreme Gradient Boosting) với mục tiêu reg:squarederror.

Để chống Overfitting (học vẹt), mình giới hạn độ sâu của cây max_depth = 4, và đặt eta = 0.03 (Learning rate thấp) để mô hình học chậm nhưng chắc chắn qua nhiều vòng lặp.

Cùng với đó là kết hợp Cross-Validation 5 folds (K-fold CV) cùng cơ chế early_stopping_rounds = 20. Mô hình sẽ tự động dừng huấn luyện nếu sau 20 vòng lặp mà sai số RMSE trên tập kiểm thử không giảm.

Nhưng dự đoán giá trị tiền tệ bằng cây quyết định thường gặp một vấn đề: Nó hay bị chệch ở các vùng giá trị quá cao hoặc quá thấp.
Do đó, các bạn có thể thấy trong code, mình tạo thêm một lớp Calibration (Hiệu chuẩn). Mình dùng hàm hồi quy tuyến tính lm(y_train ~ train_pred_raw) để nắn lại các kết quả thô (raw_pred) một lần nữa. Cuối cùng, dùng hàm exp() - 1 để đưa giá trị Logarit trả ngược về đơn vị Euro ban đầu."

PHẦN 3: SCOUTING LAYER - THUẬT TOÁN ĐÁNH GIÁ (2 Phút)
"Mô hình trả ra giá trị định giá, nhưng tiền không phải là tất cả. Việc tuyển trạch cần biết cầu thủ đó có thực sự 'ngon' hay không. Đó là lý do mình viết Phase 3: Lớp Tuyển Trạch.

Mình đã xây dựng một hàm scale01 (Min-Max Scaler) để quy đổi mọi chỉ số chuyên môn của cầu thủ trong giải đấu về thang điểm từ 0 đến 1. Sau đó, mình tính toán performance_score cho từng vị trí.
Ví dụ với Tiền vệ (Midfielders), điểm này được tính bằng: 28% Rating + 24% Kiến tạo kỳ vọng (xA) + 20% Key Pass, v.v.

Cuối cùng, hệ thống tính ra một scout_score tổng quát. Công thức này là sự kết hợp của 5 yếu tố cốt lõi mà mọi Giám đốc Thể thao đều quan tâm:

25% Value Score: Cầu thủ có đang bị định giá thấp hơn giá trị AI tính toán hay không? (value_ratio)

25% Performance: Điểm năng lực chuyên môn thuẩn túy.

20% Minutes: Tần suất ra sân (tránh mua bệnh binh).

15% Age: Tuổi tác (ưu tiên độ tuổi trẻ hơn).

15% Contract: Thời hạn hợp đồng còn lại (hợp đồng ngắn thì dễ ép giá).

Nhờ bộ lọc này, hệ thống tự động phân tách cơ sở dữ liệu hàng ngàn cầu thủ thành các danh sách mục tiêu cụ thể: undervalued_players, wonderkids (dưới 20 tuổi), và breakout_players (đang bước vào độ chín)."

PHẦN 4: DEMO TRỰC QUAN & KẾT LUẬN (1.5 Phút)
"Để trực quan hóa các con số, mình đã dùng thư viện ggplot2 và ggrepel để vẽ biểu đồ Scout Map.
(Mở/Chỉ vào plot Scout Map)

Như các bạn thấy, biểu đồ phân tán này sử dụng trục X là Value Ratio (Tỷ lệ bị định giá thấp) và trục Y là Scout Score (Điểm chất lượng). Mình đã kẻ các đường cắt (threshold) để chia thị trường thành 4 góc phần tư:

Góc xanh lá (Top Target): Vừa đá hay, vừa rẻ. Phải mua ngay lập tức.

Góc xanh dương (Hidden Gem): Khá rẻ, chuyên môn ổn. Phù hợp làm phương án dự phòng.

Góc đỏ (Overpriced Star): Đá hay nhưng giá đã bị thổi lên quá cao so với giá trị thực. Cảnh báo tránh mua hớ.

Kết luận:
Hệ thống này cho thấy, khi chúng ta đưa các định luật về vòng đời sự nghiệp, tỷ lệ dứt điểm, và các tham số tài chính vào một không gian toán học nhiều chiều của XGBoost, chúng ta hoàn toàn có thể qua mặt được các đánh giá cảm tính trên thị trường.

Trong tương lai, mình muốn tích hợp thêm một luồng dữ liệu thời gian thực (real-time API) để hệ thống tự cập nhật biến động phong độ hàng tuần, và có thể đóng gói thành một Backend Service hoàn chỉnh phục vụ cho các bộ phận phân tích của câu lạc bộ.

Cảm ơn thầy cô và các bạn đã lắng nghe!"



Hàm này dùng mô hình đã train để dự đoán giá trị cầu thủ theo từng vị trí, sau đó chuyển kết quả dự đoán thành các chỉ số phục vụ tuyển trạch như predicted value, value ratio và undervaluation score.


Kết quả cho thấy mô hình hoạt động tốt nhất với tiền vệ và tiền đạo, với R² khoảng 0.63–0.64. Trong khi đó, hậu vệ và đặc biệt là thủ môn có độ chính xác thấp hơn, do các yếu tố phòng ngự và thủ môn khó lượng hóa bằng dữ liệu hơn. Ngoài ra, số lượng dữ liệu của thủ môn cũng ít hơn, dẫn đến hiệu suất mô hình thấp hơn
