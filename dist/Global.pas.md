<!-- tabs:start -->

#### **Documentation**

# Documentation for Code Unit `Global`

## 1. Overview:

### Objective and Problem Solved:
The `Global` unit provides a set of global functions, constants, and variables that are used across the application. It centralizes common operations such as dataset manipulation, field validation, grid configuration, and utility functions. This approach reduces code duplication and ensures consistency in handling common tasks.

### High-Level Functionality:
The unit includes:
- Global constants and variables for application-wide use.
- Functions for dataset manipulation (e.g., setting fields, calculating sums).
- Validation functions for dates, values, and email formats.
- Procedures for configuring grid fields and column widths.
- Utility functions for criteria management and control state handling.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) and third-party components.
- **Third-Party Libraries**: Includes components like `cxCalendar`, `sCheckBox`, `cxGrid`, and others for UI and data handling.

### Form Type:
This unit does not define a form or grid directly. Instead, it provides utility functions and procedures that can be used in forms or grids elsewhere in the application.

---

## 2. Functionality Description:

### Actions Users or Software Can Perform:
- Manipulate datasets (e.g., set fields, calculate sums, retrieve values).
- Validate input fields (e.g., dates, email, numeric ranges).
- Configure grid fields (e.g., set column widths, disable editing).
- Manage criteria for filtering data.
- Handle control states (e.g., enable/disable controls).

### Main Components:
1. **Global Variables**: Store auxiliary strings, messages, and default values.
2. **Dataset Functions**: Perform operations like setting fields, retrieving values, and calculating sums.
3. **Validation Functions**: Validate dates, email formats, and numeric ranges.
4. **Grid Configuration Procedures**: Configure grid fields and column widths.
5. **Utility Functions**: Manage criteria and control states.

### Pseudo-Code for Actions and Events:
- **Set for Edition**: `if dataset provided then set dataset to edit mode`.
- **Validate Dates**: `if start date and end date are valid then return true else return false`.
- **Set Field Read-Only**: `if field provided then set field read-only state to specified value`.
- **Validate Email**: `if email matches regex pattern then return true else return false`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**: Global variables and constants are declared and initialized.
2. **Dataset Manipulation**: Functions like `SetForEdition` and `CDSEdition` are used to prepare datasets for editing.
3. **Validation**: Functions like `m_ValidateDates` and `ValidEmail` are called to validate user input.
4. **Grid Configuration**: Procedures like `SetColsWidthInGrid` are used to configure grid columns.
5. **Utility Functions**: Functions like `addCriteria` and `FreeServiceCriteria` are used to manage filtering criteria.

### Data Required:
- Dataset objects for manipulation.
- Field names and values for setting or retrieving data.
- Date values for validation.
- Email strings for validation.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Set for Edition**: Requires a valid dataset.
- **Validate Dates**: Requires valid start and end date fields.
- **Set Field Read-Only**: Requires a valid field object.
- **Validate Email**: Requires a non-empty email string.

### Available Filters:
- No explicit filters are defined in this unit.

### Error Messages:
- No explicit error messages are defined in this unit.

### Default Field Values:
- `gv_DefaultBusUnit`: Default value for the business unit.

### Field Validation and Conditions:
- **Email**: Validated using a regex pattern.
- **Dates**: Start and end dates must be valid and in the correct order.
- **Numeric Ranges**: Validated against minimum and maximum values.

---

## 5. Main Functions:

1. **SetForEdition**: Prepares a dataset for editing.
2. **CDSEdition**: Checks if a dataset is in edit mode.
3. **GetFieldValuesFromCDS**: Retrieves field values from a dataset.
4. **fg_CalcFieldSum**: Calculates the sum of a field in a dataset.
5. **m_ValidateDates**: Validates date ranges.
6. **ValidEmail**: Validates email format.
7. **SetColsWidthInGrid**: Configures column widths in a grid.

---

## 6. API Service Consumption:

This unit does not directly consume any external API services.

---

## 7. Conditional Fields (Form Logic):

This unit does not define any conditional fields.

---

## 8. Dependencies:

### External Libraries:
- **cxCalendar**: For date-related components.
- **sCheckBox**: For checkbox components.
- **cxGrid**: For grid components.
- **checkVatService**: Likely used for VAT validation.

### Custom Components:
- **kneConfigObjects**: Custom configuration objects.
- **kneCBListSOA**: Custom list component.
- **kneFRGridEditSOA**: Custom grid editing component.

---

## 9. Fields and Validations Listing:

This unit does not define specific fields but provides functions for validating:
- **Email**: Must match a valid email format.
- **Dates**: Must be valid and in the correct order.
- **Numeric Ranges**: Must fall within specified limits.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable as this unit provides utility functions.

### Sequence Diagram:
Not applicable as this unit provides utility functions.

### Code Snippets:
```delphi
// Example: Setting a dataset for editing
SetForEdition(MyClientDataSet);

// Example: Validating email
if ValidEmail('test@example.com') then
  ShowMessage('Valid email')
else
  ShowMessage('Invalid email');
```

### Screenshots:
Not applicable as no form or grid is defined in this unit.

---

## 11. Important Comments in the Code:

- **Global Variables**: Used for auxiliary purposes and default values.
- **SetForEdition**: Overloaded procedure for preparing datasets for editing.
- **Validation Functions**: Ensure data integrity and correctness.

---

## 12. Conclusion:

The `Global` unit is a utility module that centralizes common operations, improving code reusability and maintainability. Its strengths lie in its comprehensive dataset manipulation and validation functions. However, it lacks detailed error handling and documentation for some functions.

---

## 13. Short Summary:

The `Global` unit provides utility functions for dataset manipulation, validation, and grid configuration, centralizing common operations to enhance code reusability and maintainability. It is a foundational module for the application.#### **Global.pas**

```
unit Global;
{
 PROPRIET�RIO   : SOPORCEL 1998
 M�DULO         : $Workfile: $
 DESCRI��O      : Fun��es (fp_...) / Variaveis Globais da Aplica��o (gv_..)
 IT LOGIN       : (Login)
 LIB EXTERNAS   :
 NOTAS          :

 �LTIMA REVIS�O :
 --------------------------------------------------------------------------
 Nr. : $Revision: 1.26 $
 DATA / HORA : $Date: 2025/02/06 10:00:50 $
 AUTOR: $Author: ibsantos $

 OUTRAS REVIS�ES:

 Nr. ,  Data,  Projecto do VSS, Coment�rios
 --------------------------------------------------------------------------
 $History: $
 $NoKeywords: $
}

interface

uses SysUtils, Classes, DB, DBTables, DBGrids, Graphics, IniFiles, Controls,
     WinTypes, ExtCtrls, Dialogs, Variants,cxCalendar, sCheckBox, DBClient,
     kneConfigObjects, cxDBEdit, cxGridDBTableView, kneCBListSOA, sDBEdit,
     kneFRGridEditSOA, cxGrid, cxEditRepositoryItems, cxImageComboBox
     , StdCtrls  ,checkVatService  , XSBuiltIns; 

{ Declara��o Tipos Globais  }
{ type}

{ Declara��o Constantes  Globais  }
const
  gc_POSTCODEKEY = 'POSTFORMAT_';

  gc_SEPARATOR: string = ';';

 {Declara��o Variaveis  Globais}
var
 gv_Str             : String;      {String Auxiliar}
 gv_Msg             : String;      {String para mensagens}
 gv_DefaultBusUnit  : String;      {Valor default do Business Unit}
 gv_BusUnitList     : String;

  procedure SetForEdition(lv_CDS: TClientDataSet);    overload;
  procedure SetForEdition(pv_DS: TDataSet);           overload;
  function CDSEdition(lv_CDS: TClientDataSet): Boolean;
  function GetFieldValuesFromCDS(const pv_dataset: TDataset;
           const pv_fieldName: string; pv_Separador: String = '|'): string;
  function fg_CalcFieldSum(
    const pv_dataset: TDataset; const pv_fieldName: string; pv_SelectFieldName: string): Currency;
  function GetFieldsValuesFromCDS(const pv_dataset: TDataset;
    const pv_fieldsName: string; pv_Values: TStringList; pv_Separador: String = '|'): Boolean;

  function fg_SetFieldWithValue(const pv_dataset: TClientDataSet;
    const pv_fieldName, pv_Value: string; pv_Filter: string = '';
    pv_SelectFieldName: string = ''; pv_SelectValue: string = ''): Boolean;
  //---
  function GetDateTimeFromServer: TDateTime;

  procedure m_SetCheckDateClick(pv_Check: TsCheckBox;
    pv_Date1, pv_Date2: TcxDateEdit);
  function m_ValidateDates(pv_IniDt, pv_FimDt: TcxDateEdit): Boolean;    overload;
  function m_ValidateDates(pv_IniDt, pv_FimDt: TcxDBDateEdit): Boolean;  overload;

  Function ValidateValues(pv_Min, pv_Max: TcxDBMaskEdit; lv_cds: TClientDataSet): Boolean;

  procedure SetNoEdittingInGridFields(pv_Fields : string; pv_form: TFORMkneCBListSOA);  overload;
  procedure SetNoEdittingInGridFields(pv_Fields : string; pv_form: TFRAMEBaseGridEditSOA);  overload;

  procedure SetColsWidthInGrid(pv_colsWidth: String; pv_gridView: TcxGridDBTableView);
  Function m_ValidateValues(pv_dataset:Tdataset; pv_limInf, pv_limSup: TsDBEdit): Boolean;

  function SetFieldReadOnlyState(const pv_Field: TField; pv_State:Boolean): Boolean;

  function SetFocusinFieldGrid(pv_FieldName: String; pv_GRD: TcxGrid;
    pv_GView: TcxGridDBTableView): Boolean;

  function ValidEmail(email: string): boolean;

//  procedure SetProtectFieldsState(Sender: TFRAMEBaseGridEditSOA);
//
//  function  SetAllControlsState(const pv_Control: TControl; const pv_Enabled: Boolean): Boolean;
//
//  procedure DoProtectControl(Sender: TObject; const pv_Args: array of const);
//

// ------------- Criteria ------------------------------------------------------
function addCriteria(var
  pv_criteria: TArrayOfFieldCriteria; pv_logical, pv_field, pv_operator,
  pv_value: string): Boolean;

function FreeServiceCriteria(pv_criteria: TArrayOfFieldCriteria):
  TArrayOfFieldCriteria;

// ------------- Utils ---------------------------------------------------------
procedure AddTocxImgCombobox(pv_Combo: TcxDBImageComboBox;
```

#### **Global.dfm**

```
dfm file is not needed for this file```
<!-- tabs:end -->

