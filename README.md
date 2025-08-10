# XFC TOOLKIT - ABAP Utility Class Collection

This repository contains a collection of reusable utility classes (`TOOLKIT`) designed to simplify and standardize common tasks in SAP ABAP projects. The goal is to reduce repetitive code, accelerate development, and maintain a consistent codebase.

## Features

  * **Easy to Use:** Each class focuses on a specific task with simple and clear methods.
  * **Code Standardization:** Use the same class for the same functionality across different projects.
  * **Modular Design:** Each toolkit is independent and can be used on its own.
  * **Extensible Design:** Classes can be easily extended by adding new methods or by creating new toolkits.

-----

## Included Toolkits
| Class Name | Description |
| :--- | :--- |
| **ZCL_XFC_BDC_TOOLKIT** | Utilities for Batch Data Communication (BDC) automation and management. |
| **ZCL_XFC_CONV_TOOLKIT** | Conversion helpers, likely for data type and format conversions. |
| **ZCL_XFC_FILE_TOOLKIT** | File operations on the application server (read, write, manage files). |
| **ZCL_XFC_GUI_TOOLKIT** | Helper functions for SAP GUI screens and user interface operations. |
| **ZCL_XFC_GW_TOOLKIT** | Utilities for SAP Gateway (OData) service operations. |
| **ZCL_XFC_IOC_TOOLKIT** | Implements Inversion of Control (IOC) principles for flexible dependency management. |
| **ZCL_XFC_LOG_TOOLKIT** | Logging and tracking of application messages and errors. |
| **ZCL_XFC_LRU_CACHE_TOOLKIT** | LRU (Least Recently Used) cache mechanism for efficient memory usage. |
| **ZCL_XFC_LRU_LINKED_LIST** | Internal linked list structure for LRU cache implementation. |
| **ZCL_XFC_LRU_NODE** | Node structure used in LRU cache linked list. |
| **ZCL_XFC_NR_TOOLKIT** | Utilities for SAP number range object management. |
| **ZCL_XFC_SYS_TOOLKIT** | System helpers, such as retrieving system information. |
| **ZCL_XFC_TABLE_TOOLKIT** | Table operations and utilities for internal tables. |
| **ZCL_XFC_TEXT_TOOLKIT** | Tools for processing and managing text objects. |
| **ZCL_XFC_UNIT_OF_WORK_TOOLKIT** | Implements the Unit of Work pattern for transactional operations. |
| **ZCL_XFC_USER_TOOLKIT** | User information and authorization utilities. |
| **ZCX_XFC_TOOLKIT_ERROR** | Custom exception class for error handling in the toolkit. |

*Note: The **ZCL\_XFC\_LRU\_LINKED\_LIST** and **ZCL\_XFC\_LRU\_NODE** classes are internal components of the LRU cache toolkit.*

-----

## Installation

You can integrate these classes into your project using one of two methods:

### Via ABAPGit

1.  Install **ABAPGit** in your SAP system.
2.  Use **ABAPGit** to clone this repository.

### Manually

1.  Download the class code from this repository.
2.  Manually import and activate each class in your system using transaction `SE24` or `SE80`.

-----

## Contributing

Your feedback and contributions are highly welcome\! Feel free to open an `Issue` or submit a `Pull Request` for bug reports, new features, or improvements.

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.