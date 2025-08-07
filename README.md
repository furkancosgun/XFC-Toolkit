# XFC\_TOOLKIT - ABAP Utility Class Collection

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
| **ZCL\_XFC\_BDC\_TOOLKIT** | Tools for the automation and management of Batch Data Communication (BDC) transactions. |
| **ZCL\_XFC\_FILE\_TOOLKIT** | Tools for file system operations on the application server (read, write). |
| **ZCL\_XFC\_GUI\_TOOLKIT** | Helper functions for SAP GUI screens (dynpro) and user interface interactions. |
| **ZCL\_XFC\_GW\_TOOLKIT** | Tools to simplify operations related to SAP Gateway (OData) services. |
| **ZCL\_XFC\_IOC\_TOOLKIT** | Helper tools for implementing the Inversion of Control (IOC) principle. |
| **ZCL\_XFC\_LOG\_TOOLKIT** | Tools for logging and monitoring application messages and errors. |
| **ZCL\_XFC\_LRU\_CACHE\_TOOLKIT** | An LRU (Least Recently Used) cache mechanism that automatically removes the least-used elements. |
| **ZCL\_XFC\_NR\_TOOLKIT** | Tools for SAP number range objects. |
| **ZCL\_XFC\_SYS\_TOOLKIT** | General system utilities, such as system information. |
| **ZCL\_XFC\_TEXT\_TOOLKIT** | Tools for processing text objects. |
| **ZCL\_XFC\_USER\_TOOLKIT** | Tools for managing user information and authorizations. |

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