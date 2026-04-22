# KẾ HOẠCH XÂY DỰNG SLIDE THUYẾT TRÌNH (BẢN RÚT GỌN 8 SLIDE)
**Học phần:** Lập trình Phân tích Dữ liệu (Programming for Data Analysis)
**Đề tài:** Data Pipeline Toàn diện: Từ Crawling Dữ liệu đến Định giá Cầu thủ bằng XGBoost

*(Bản này được thiết kế tối ưu, ngắn gọn, súc tích trong đúng 8 Slide. Phù hợp để trình bày lướt qua phương pháp luận, sau đó tập trung thời gian biểu diễn chạy Code thật (Demo) ăn điểm tuyệt đối).*

---

### Slide 1: Tiêu đề & Giới thiệu (Title Slide)
- **Visuals:** Tên đề tài, Tên nhóm/Cá nhân thực hiện, Tên môn học.
- **🗣️ Kịch bản nói:** *"Xin chào thầy cô. Hôm nay nhóm em xin trình bày đồ án 'Phân tích Dữ liệu và Định giá Cầu thủ'. Thay vì chỉ dùng 1 tool, đồ án này là một chu trình dữ liệu hoàn chỉnh (End-to-End Pipeline): Kết hợp ngôn ngữ Python để thu thập cào dữ liệu thô (crawling) và ngôn ngữ R để phân tích, làm sạch, và xây dựng mô hình AI dự đoán giá trị cầu thủ."*

### Slide 2: Bài toán & Mục tiêu (Objectives)
- **Visuals:** Diagram 3 bước: Thu thập (Python) ➡️ Xử lý & AI (R) ➡️ Kết quả (Scouting).
- **🗣️ Kịch bản nói:** *"Bài toán nhóm đặt ra là làm sao tìm được những món hời trên thị trường chuyển nhượng. Mục tiêu của đồ án để giải quyết bài toán đó gồm 3 bước: Bẻ khóa API lấy dữ liệu thô -> Xử lý số liệu và tạo các chỉ số thông minh -> Cuối cùng là huấn luyện thuật toán Machine Learning XGBoost để phát hiện cầu thủ định giá thấp (Undervalued)."*

### Slide 3: Thu thập Dữ liệu (Python Crawling)
- **Visuals:** Chụp code Python hiển thị `ThreadPoolExecutor`, đổi IP `PROXIES_LIST` và màn hình báo lỗi 403 của Statshub.
- **🗣️ Kịch bản nói:** *"Bọn em lấy dữ liệu trực tiếp từ API của Statshub. Thách thức lớn nhất là cơ chế chặn bot (Anti-bot) rất gắt. Nếu gửi request liên tục sẽ bị báo lỗi 429 và cấm IP. Để vượt qua, nhóm đã code Python kết hợp Đa luồng (Multi-threading) cào song song và Xoay vòng Proxy (đổi IP liên tục mỗi 30s). Kết quả thu về file CSV với hơn 20.000 cầu thủ."*

### Slide 4: Làm sạch & Tạo đặc trưng (R - Feature Engineering)
- **Visuals:** Cắt ghép 2 đoạn code R: Hàm `safe_div` và đoạn `mutate` tạo `finishing_efficiency`.
- **🗣️ Kịch bản nói:** *"Dữ liệu thô đưa vào R có nhiều rác và giá trị bị khuyết (NA). Bọn em dùng `dplyr` để làm sạch. Quan trọng nhất, AI không tự hiểu bóng đá, nên nhóm phải tạo ra các Siêu chỉ số bằng nghiệp vụ. Ví dụ tính 'Hiệu suất dứt điểm' bằng Bàn thắng chia xG. Kết hợp hàm `log1p` và ép giới hạn `cap_quantile` để xử lý các số liệu dị thường (Outliers), giúp mô hình không bị thiên vị."*

### Slide 5: Xây dựng Mô hình XGBoost
- **Visuals:** Chụp cấu hình `xgb.cv` và hiển thị list `feature_map` tách làm 4 mảng F, M, D, G.
- **🗣️ Kịch bản nói:** *"Nhóm train 4 mô hình XGBoost tách biệt cho 4 vị trí: Tiền đạo, Tiền vệ, Hậu vệ, Thủ môn vì tiêu chí định giá của họ hoàn toàn khác nhau. Quá trình huấn luyện dùng Cross-Validation 5-fold và cài đặt `early_stopping_rounds = 20`. Tức là nếu qua 20 vòng lặp mà AI học không tiến bộ lên thì tự động ngắt để chống học vẹt (Overfitting)."*

### Slide 6: Đánh giá & Giải mã Mô hình
- **Visuals:** Chụp bảng kết quả Console có R² và Bảng Feature Importance.
- **🗣️ Kịch bản nói:** *"Để đánh giá, nhóm sử dụng chỉ số R-squared, cho thấy mô hình có sức mạnh dự báo lên tới [đọc số R2] %. Ngoài ra, nhìn vào bảng giải mã Feature Importance của Tiền Đạo, AI tự động nhận diện được Tuổi tác và Số bàn thắng kỳ vọng (npxG) là hai yếu tố quyết định tới giá trị chuyển nhượng cao nhất."*

### Slide 7: Bản đồ Tuyển trạch (Scout Map Quadrants)
- **Visuals:** CHỤP TOÀN MÀN HÌNH BIỂU ĐỒ SCOUT MAP (Biểu đồ chia 4 ô vuông F, M, D, G với 4 màu).
- **🗣️ Kịch bản nói:** *"Bọn em thiết kế thêm một tầng quy tắc chấm điểm (Scouting Rule-based) tự động. Cầu thủ sẽ được chấm theo thang điểm 1.0. Đỉnh cao của đồ án nằm ở Bản đồ trực quan này: Trục X là Giá, Trục Y là Hiệu suất. Góc màu Xanh lá là 'Top Target' - đá hay mà rẻ, cần mua ngay. Góc màu Đỏ là 'Overpriced' - giá đắt phi lý. Biểu đồ gán nhãn tự động những cái tên sáng giá nhất."*

### Slide 8: Demo Code Trực Tiếp (Live Demonstration)
- **Visuals:** Dòng chữ "LIVE DEMONSTRATION IN RSTUDIO" cực lớn.
- **🗣️ Kịch bản nói:** *"Lý thuyết là như vậy. Giờ em xin phép chạy trực tiếp mã nguồn R để thầy cô xem hệ thống vận hành thực tế. Hệ thống sẽ cắn dữ liệu và xuất ra Top 10 cầu thủ đáng mua nhất (Undervalued) chỉ trong vài giây. Xin mời thầy cô hướng lên màn hình..."*

*(**Sau câu này:** Bạn mở RStudio, mở `Alo.r`, quét toàn bộ khối code hoặc ấn nút Source. Màn hình console sẽ nháy liên tục in ra các bảng dữ liệu kết quả, và tab Plots sẽ hiện ra Biểu đồ siêu ngầu. Kết thúc bài bảo vệ rực rỡ!).*
