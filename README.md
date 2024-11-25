# Bulk Upload & Validation - RShiny 📚📤🏅

![GitHub Repo stars](https://img.shields.io/github/stars/Soumyadipta2020/Bulk_Upload_and_validation-RShiny?style=social)
![GitHub forks](https://img.shields.io/github/forks/Soumyadipta2020/Bulk_Upload_and_validation-RShiny?style=social)
![GitHub license](https://img.shields.io/github/license/Soumyadipta2020/Bulk_Upload_and_validation-RShiny)
[![HitCount](https://hits.dwyl.com/Soumyadipta2020/Bulk_Upload_and_validation-RShiny.svg?style=flat-square)](http://hits.dwyl.com/Soumyadipta2020/Bulk_Upload_and_validation-RShiny)

This RShiny application allows users to bulk upload files, validate their formats based on predefined categories, and view the validation results in a user-friendly table.

## ⭐ Features

- **Bulk File Upload**: Upload multiple files at once.
- **Category-Based Validation**: Verify file formats according to a selected category.
- **Real-Time Feedback**: View the validation status of each uploaded file in a summary table.

## 🚩 Requirements

- **R** (version 4.4.0 or higher)
- **RShiny** (version 2024.09.1 or higher)
- Other dependencies:
  - `shiny`
  - `dplyr`
  - `DT`
  - `bslib`

## 📲 Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/Soumyadipta2020/Bulk_Upload_and_validation-RShiny.git
   cd Bulk_Upload_and_validation-RShiny
   ```

2. Install required packages in R:
   ```r
   install.packages(c("shiny", "dplyr", "DT", "bslib))
   ```

## 👨‍💻 Usage

1. Run the RShiny app:
   ```r
   shiny::runApp("app.R")
   ```

## 💁 How It Works

1. **Upload Files**: Select files for bulk upload.
2. **Choose Category**: Select the appropriate file category for validation.
3. **Validation Process**: The app checks each file's format against the selected category's criteria.
4. **Results Table**: A summary table displays each file's validation status.

## 🏅 Example Validation Categories

- **Category A**: `.csv` files with specific columns

## 💡 Contribution

Contributions are welcome! If you have ideas to enhance the app or fix issues, feel free to fork the repository, make changes, and submit a pull request.

Steps to Contribute:

1. Fork this repository.
2. Create a new branch: `git checkout -b feature-name`
3. Commit your changes: `git commit -m "Add feature-name"`
4. Push to your branch: `git push origin feature-name`
5. Open a Pull Request.
