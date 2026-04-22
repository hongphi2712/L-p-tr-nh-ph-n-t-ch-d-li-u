# KẾ HOẠCH XÂY DỰNG SLIDE THUYẾT TRÌNH (KỊCH BẢN TỪ A-Z CHO NGƯỜI NÓI)
**Học phần:** Lập trình Phân tích Dữ liệu (Programming for Data Analysis)
**Đề tài:** Data Pipeline Toàn diện: Từ Crawling Dữ liệu đến Định giá Cầu thủ bằng XGBoost

*(Bản này được thiết kế để bạn làm slide cực nhàn. Trên slide chỉ cần để text ngắn gọn và chèn ảnh. Còn bạn cầm giấy đọc thuộc y hệt phần **🗣️ Kịch bản nói (Speaker Notes)** là đảm bảo trôi chảy, logic 10/10).*

---

## PHẦN 1: MỞ ĐẦU & TỔNG QUAN

### Slide 1: Tiêu đề (Title Slide)
- **Tiêu đề Slide:** Phân tích Dữ liệu và Xây dựng Mô hình Định giá Cầu thủ bằng R & XGBoost.
- **🗣️ Kịch bản nói:** *"Xin chào thầy cô và các bạn. Nhóm chúng em xin trình bày đồ án 'Phân tích Dữ liệu và Định giá Cầu thủ Bóng đá'. Đồ án này là một chu trình dữ liệu hoàn chỉnh (End-to-End Pipeline), kết hợp giữa lập trình Python để thu thập cào dữ liệu (crawling) và lập trình R để phân tích, xây dựng mô hình AI dự đoán giá trị cầu thủ."*

### Slide 2: Mục tiêu đồ án (Objectives)
- **Visuals:** Các bullet points liệt kê mục tiêu (Thu thập data -> Làm sạch -> Feature Engineering -> Model XGBoost -> Scouting).
- **🗣️ Kịch bản nói:** *"Mục tiêu của đồ án không chỉ là làm ra một bảng xếp hạng bóng đá vui vẻ. Chúng em muốn chứng minh năng lực lập trình để giải quyết một bài toán dữ liệu lớn: Bắt đầu từ việc bẻ khóa API để cào dữ liệu thô, dùng R để làm sạch, trích xuất đặc trưng và cuối cùng là huấn luyện thuật toán Machine Learning để tìm ra những món hời trên thị trường chuyển nhượng."*

---

## PHẦN 2: THU THẬP DỮ LIỆU (CRAWLING TỪ STATSHUB BẰNG PYTHON)

### Slide 3: Thách thức khi thu thập dữ liệu (Data Scraping Challenges)
- **Visuals:** Chụp ảnh giao diện web statshub.com và hình chụp báo lỗi 403 Forbidden / 429 Too Many Requests.
- **🗣️ Kịch bản nói:** *"Nguồn dữ liệu của nhóm được lấy trực tiếp từ API của trang Statshub. Thách thức cực lớn ở đây là hệ thống có cơ chế Anti-bot rất gắt. Nếu gửi request liên tục để lấy thông tin của hàng chục ngàn cầu thủ, server sẽ lập tức trả về lỗi 403 hoặc 429 và ban IP của chúng ta vĩnh viễn."*

### Slide 4: Giải pháp Kỹ thuật: Multi-threading & Proxy Rotation
- **Visuals:** Chụp đoạn code Python chứa `ThreadPoolExecutor`, `PROXIES_LIST`, và `USER_AGENTS`.
- **🗣️ Kịch bản nói:** *"Để vượt qua bức tường lửa này, chúng em đã viết script Python ứng dụng 3 kỹ thuật cốt lõi: Thứ nhất là Đa luồng (Multi-threading) để cào dữ liệu song song với tốc độ cao. Thứ hai là Xoay vòng Proxy (Proxy Rotation) - cứ 30 giây kịch bản sẽ test lại dàn Proxy sống chết ra sao để đổi IP liên tục. Thứ ba là fake User-Agents. Nhờ vậy, nhóm đã thu về thành công tập tin CSV thô với hơn 20.000 cầu thủ."*

---

## PHẦN 3: DATA WRANGLING & FEATURE ENGINEERING (BẰNG R)

### Slide 5: Làm sạch dữ liệu (Data Cleaning)
- **Visuals:** Chụp khối code `mutate` có dùng `as_datetime()` và `replace_na()`.
- **🗣️ Kịch bản nói:** *"Sau khi có CSV thô, chúng em đẩy vào ngôn ngữ R. Ở bước làm sạch, chúng em dùng package `lubridate` để ép kiểu ngày sinh, từ đó quy ra số Tuổi chính xác. Những cột thông số chuyên môn bị khuyết thiếu (NA) do cầu thủ không thi đấu sẽ được gán bằng 0 bằng hàm `replace_na` để đảm bảo mô hình không bị sập (crash) khi tính toán."*

### Slide 6: Xử lý Toán học - Tại sao lại dùng Log1p?
- **Visuals:** Chụp ảnh hàm `safe_div` và đoạn code `goalsP90_log = log1p(goalsP90)`.
- **🗣️ Kịch bản nói:** *"Một kỹ thuật xử lý quan trọng nhóm áp dụng là Log Transform. Dữ liệu bóng đá có rất nhiều số 0 (như 0 bàn thắng). Nếu dùng `log` bình thường sẽ sinh ra lỗi Vô cực âm (-Inf). Nên nhóm dùng hàm `log1p`, bản chất là `log(1 + x)` để xử lý an toàn. Việc dùng Log giúp bóp gọn phân phối lệch phải của các siêu sao trăm triệu Euro, đưa biểu đồ về dạng phân phối chuẩn để AI học không bị thiên vị."*

### Slide 7: Trích xuất Đặc trưng (Composite Features & Capping)
- **Visuals:** Chụp đoạn code tính `finishing_efficiency` và hàm `cap_quantile`.
- **🗣️ Kịch bản nói:** *"AI không tự hiểu bóng đá, chúng em phải dạy nó bằng cách tạo ra các siêu chỉ số thông qua `mutate`. Ví dụ: 'Hiệu suất dứt điểm' bằng Bàn thắng chia cho xG (bàn thắng kỳ vọng). Kèm theo đó, nhóm tự code hàm `cap_quantile` để gọt bỏ các giá trị dị thường (Outliers) ở mức 1% và 99%. Ví dụ một cầu thủ trẻ đá 10 phút ghi 1 bàn sẽ bị hệ thống quy đổi thành 9 bàn/trận, hàm Capping này sẽ ép chỉ số ảo đó xuống mức thực tế để chống nhiễu."*

---

## PHẦN 4: XÂY DỰNG MÔ HÌNH MACHINE LEARNING (XGBOOST)

### Slide 8: Tách biệt Mô hình (Model Separation)
- **Visuals:** Chụp list `feature_map` phân tách F, M, D, G.
- **🗣️ Kịch bản nói:** *"Nhóm quyết định train 4 mô hình XGBoost tách biệt hoàn toàn cho 4 vị trí: Tiền đạo, Tiền vệ, Hậu vệ, Thủ môn. Lý do vì Tiền đạo được định giá bằng Số bàn thắng, trong khi Hậu vệ định giá bằng Tắc bóng. Nếu gộp chung lại ném cho AI, thuật toán sẽ bị nhiễu trọng số và gán giá trị sai lầm."*

### Slide 9: Kỹ thuật Cross-Validation & Chống Overfitting
- **Visuals:** Chụp đoạn hàm `xgb.cv` có chứa `early_stopping_rounds`.
- **🗣️ Kịch bản nói:** *"Quá trình Train sử dụng kỹ thuật Cross-Validation 5-fold để đánh giá chéo. Đặc biệt nhóm cài đặt tham số `early_stopping_rounds = 20`. Tức là nếu qua 20 vòng lặp mà sai số trên tập Validation không thể giảm xuống được nữa thì AI sẽ tự động ngắt. Đây là thủ thuật cực kỳ hiệu quả trong Lập trình Máy học để chống hiện tượng Học vẹt (Overfitting)."*

### Slide 10: Calibration & Đánh giá Metrics (RMSE, R²)
- **Visuals:** CHỤP ẢNH Console kết quả của bảng `model_eval`.
- **🗣️ Kịch bản nói:** *"Kết quả dự đoán của XGBoost đôi lúc bị lệch đường cong, nhóm dùng thêm 1 mô hình tuyến tính nhỏ (`lm`) để hiệu chỉnh lại (Calibration). Kết quả đánh giá bằng R-squared cho thấy mức độ giải thích của mô hình lên tới [đọc số R2 trên màn hình] %, chứng minh khả năng dự đoán cực kì mạnh mẽ."*

### Slide 11: Giải mã hộp đen (Feature Importance)
- **Visuals:** CHỤP ẢNH Console output của bảng Feature Importance.
- **🗣️ Kịch bản nói:** *"Để giải thích vì sao AI lại đưa ra mức giá đó, nhóm xuất ra bảng Feature Importance. Như thầy cô thấy trên bảng của Tiền đạo, tuổi tác (age) và số bàn thắng kỳ vọng (npxG) là những đặc trưng chiếm trọng lượng lớn nhất quyết định tới giá trị cầu thủ."*

---

## PHẦN 5: ỨNG DỤNG VÀ TRỰC QUAN HÓA (SCOUTING LAYER)

### Slide 12: Biểu đồ Tương quan Dự đoán vs Thực tế
- **Visuals:** CẮT ẢNH PLOT Scatter plot (Predicted vs Market) có vẽ đường chéo đỏ.
- **🗣️ Kịch bản nói:** *"Biểu đồ phân tán này trực quan hóa sai số của mô hình. Trục X là giá thị trường, Trục Y là giá mô hình dự báo. Điểm xanh càng bám sát đường đứt nét màu đỏ thì dự báo càng chuẩn. Những điểm nằm xa đường màu đỏ và vút lên trên chính là mỏ vàng - những cầu thủ đang bị định giá quá thấp so với năng lực thực sự."*

### Slide 13: Xây dựng Hệ thống Scouting Layer
- **Visuals:** Chụp khối code tính toán `scout_score`.
- **🗣️ Kịch bản nói:** *"Chạy xong AI là chưa đủ, nhóm thiết lập thêm Hệ thống Scouting Rule-based để tự động ra quyết định. Cầu thủ được chấm theo thang 1.0 điểm, trong đó trọng số gán cho: 25% độ chênh lệch giá tiền, 25% hiệu suất trên sân, 20% khả năng ra sân thường xuyên, và 15% là ưu tiên độ tuổi trẻ."*

### Slide 14: Bản đồ Tuyển trạch (Scout Map Quadrants)
- **Visuals:** CHỤP ẢNH BIỂU ĐỒ SCOUT MAP (Biểu đồ chia 4 ô vuông F, M, D, G với 4 màu Xanh, Đỏ, Cam, Xám).
- **🗣️ Kịch bản nói:** *"Đỉnh cao của trực quan hóa trong đồ án này là Bản đồ tuyển trạch. Dùng package `ggplot2` và `ggrepel`, nhóm chia dữ liệu thành 4 góc phần tư: 
  - Vùng **Màu Xanh lá (Top Target)** là những món hời: Đá hay mà giá cực rẻ.
  - Vùng **Màu Đỏ (Overpriced Star)** là ngôi sao bị ngáo giá: Đá hay nhưng giá đắt phi lý.
  - Biểu đồ tự động gán tên những cầu thủ tiêu biểu nhất của từng vùng lên màn hình mà không bị đè chữ."*

### Slide 15: Demo Code Trực Tiếp & Kết Luận
- **Visuals:** Dòng chữ "LIVE DEMONSTRATION IN RSTUDIO" thật lớn trên slide.
- **🗣️ Kịch bản nói:** *"Dạ và để chứng minh hệ thống hoạt động thực tế thế nào, xin mời thầy cô nhìn lên màn hình. Em sẽ chạy trực tiếp file code `Alo.r` trên RStudio. (BẠN BẤM QUÉT KHỐI VÀ CHẠY SOURCE TRONG RSTUDIO). 
  Như thầy cô thấy, hệ thống chỉ mất vài giây để nhai nốt 20.000 dữ liệu, chạy huấn luyện AI và tự động in ra màn hình Console danh sách Top 10 Cầu Thủ Undervalued (Ngon - Bổ - Rẻ) đáng mua nhất ngay lúc này. Đồ án của chúng em xin kết thúc tại đây, xin cảm ơn thầy cô."*
