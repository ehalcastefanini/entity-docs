<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustBia` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRcustBia` code unit defines a form (`TFRAMEcustBia`) that is part of a Delphi application. This form is designed to manage customer-related data, specifically focusing on sales percentages, reference days, tolerance days, and rebate information. It provides a user interface for inputting and managing these details, ensuring that the data is properly initialized and validated.

### Technologies Used:
- **Delphi VCL (Visual Component Library):** Used for creating the user interface and handling events.
- **SOAP Services:** Used for potential integration with external services.
- **Database Components:** Includes `TDataSet` and `TClientDataSet` for database interaction.
- **Third-party Libraries:** Includes components like `TsLabel`, `TsDBEdit`, `TcxDBMaskEdit`, and `TcxDBImageComboBox` for enhanced UI and functionality.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types:**
  - Labels (`TsLabel`): Display static text.
  - Text Fields (`TsDBEdit`, `TcxDBMaskEdit`): Input fields for numeric and text data.
  - Combo Box (`TcxDBImageComboBox`): Dropdown for selecting modes.
  - Bevel (`TsBevel`): Visual separator.
- **Form Actions and Effects:**
  - `CDStableNewRecord`: Initializes default values for new records.
  - `m_InsertNewRecord`: Custom logic for inserting new records.
  - `SetForDOCADDR`: Custom logic for setting document address-related properties.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can input and manage customer-related data such as sales percentages, reference days, tolerance days, and rebates.
- The form initializes default values for new records and applies specific styles to input fields.

### Main Components:
- **Labels (`TsLabel`):** Provide context for input fields.
- **Input Fields (`TsDBEdit`, `TcxDBMaskEdit`):** Allow users to input numeric and text data.
- **Combo Box (`TcxDBImageComboBox`):** Allows users to select a mode.
- **Bevel (`TsBevel`):** Enhances the visual layout.

### Pseudo-code for Actions and Events:
- `OnCreate` event of the form: 
  ```
  if form is created then
    initialize properties and styles
  ```
- `OnNewRecord` event of the dataset:
  ```
  if new record is created then
    initialize default field values
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The form is created, and its properties (`MasterKeyFields`, `DataPacketName`, etc.) are initialized.
   - Styles are applied to input fields.
2. **User Interaction:**
   - Users input data into fields or select options from the combo box.
   - When a new record is created, default values are applied.
3. **Functions:**
   - `Create` (File: `FRcustBia.pas`): Initializes the form and its properties.
   - `CDStableNewRecord` (File: `FRcustBia.pas`): Sets default values for new records.

### Required Data:
- Sales percentages, reference days, tolerance days, rebate values, and mode selection.

---

## 4. Business Rules:

### Actions and Preconditions:
- **New Record Initialization:** Automatically applies default values when a new record is created.
- **Field Input:** Fields must be filled with valid data before saving.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Default values are applied using `TkneDB.InitializeFieldDefaults`.

### Field Validation and Conditions:
- Not explicitly defined in the code.

---

## 5. Main Functions:

1. **`Create`:** Initializes the form and its properties.
2. **`CDStableNewRecord`:** Sets default values for new records.
3. **`m_InsertNewRecord`:** Custom logic for inserting new records.
4. **`SetForDOCADDR`:** Custom logic for setting document address-related properties.

---

## 6. API Service Consumption:

- No explicit API calls are defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **`TsLabel`, `TsDBEdit`, `TcxDBMaskEdit`, `TcxDBImageComboBox`:** Used for UI components.
- **`TkneDB`:** Used for initializing default field values.

### Custom Components:
- **`TFRAMEBaseCtrlEditSOA`:** Base class for the form.
- **`FRAMEstatusInfo1`:** Custom frame for displaying status information.

---

## 9. Fields and Validations Listing:

### Fields:
1. **Stock Sales Percentage (`EDTstockSalesPerc`):** Type: Numeric, Required.
2. **Direct Sales Percentage (`EDTdirectSalesPerc`):** Type: Numeric, Required.
3. **Reference Days (`EDTreferenceDays`):** Type: Numeric, Required.
4. **Tolerance Days (`EDTtoleranceDays`):** Type: Numeric, Required.
5. **Rebate All (`EDTrebAll`):** Type: Numeric, Optional.
6. **Rebate CutSize (`EDTrebCutSize`):** Type: Numeric, Optional.
7. **Rebate Folio (`EDTrebFolio`):** Type: Numeric, Optional.
8. **Rebate Reels (`EDTrebReels`):** Type: Numeric, Optional.
9. **Mode (`ICBOmode`):** Type: Dropdown, Required.

### Mapping:
- Not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Form Initialization] --> [Set Properties and Styles] --> [User Input] --> [New Record Creation] --> [Apply Default Values]
```

### Sequence Diagram:
```plaintext
User --> Form: Input Data
Form --> Dataset: Create New Record
Dataset --> Form: Apply Default Values
```

### Code Snippets:
```delphi
procedure TFRAMEcustBia.CDStableNewRecord(DataSet: TDataSet);
begin
  TkneDB.InitializeFieldDefaults(CDStable);
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="width: 791px; font-family: Verdana;">
  <label style="position: absolute; left: 8px; top: 16px;">Period:</label>
  <label style="position: absolute; left: 166px; top: 16px;">To:</label>
  <label style="position: absolute; left: 8px; top: 39px;">Stock Sales:</label>
  <label style="position: absolute; left: 166px; top: 39px;">Direct Sales:</label>
  <label style="position: absolute; left: 326px; top: 39px;">Ref. Days:</label>
  <label style="position: absolute; left: 476px; top: 39px;">Tolerance Days:</label>
  <label style="position: absolute; left: 8px; top: 65px;">Rebates</label>
  <label style="position: absolute; left: 42px; top: 117px;">All:</label>
  <label style="position: absolute; left: 168px; top: 117px;">CutSize:</label>
  <label style="position: absolute; left: 294px; top: 117px;">Folio:</label>
  <label style="position: absolute; left: 400px; top: 117px;">Reels:</label>
</div>
```

---

## 11. Important Comments in the Code:

- **Initialization of Default Values:**
  ```delphi
  TkneDB.InitializeFieldDefaults(CDStable);
  ```
- **Style Application:**
  ```delphi
  EDTrebAll.Style.StyleController := DMODskin.cxEditStyles1;
  ```

---

## 12. Conclusion:

The `FRcustBia` code unit provides a structured form for managing customer-related data. It initializes default values, applies styles, and ensures a user-friendly interface. However, it lacks explicit error handling and field validation, which could be improved.

---

## 13. Short Summary:

The `FRcustBia` form manages customer data, including sales percentages, reference days, and rebates. It initializes default values and applies styles but lacks explicit error handling and validation. It is part of a larger Delphi application for customer management.#### **FRcustBia.pas**

```
unit FRcustBia;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  kneFRStatusInfo, cxDBEdit, cxControls, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxCalendar, sFrameAdapter, sBitBtn, sPanel,
  sDBEdit, sLabel, cxGraphics, cxImageComboBox, sBevel;

type
  TFRAMEcustBia = class(TFRAMEBaseCtrlEditSOA)
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBLInicialPeriod: TsLabel;
    LBLEndPeriod: TsLabel;
    Label1: TsLabel;
    Label2: TsLabel;
    Label3: TsLabel;
    Label4: TsLabel;
    EDTstockSalesPerc: TsDBEdit;
    EDTreferenceDays: TsDBEdit;
    EDTdirectSalesPerc: TsDBEdit;
    EDTtoleranceDays: TsDBEdit;
    EDTperiodoIni: TsDBEdit;
    EDTperiodoFim: TsDBEdit;
    LBLrebate: TsLabel;
    LBLall: TsLabel;
    LBLcutSize: TsLabel;
    LBLfolio: TsLabel;
    LBLreels: TsLabel;
    EDTrebAll: TcxDBMaskEdit;
    EDTrebCutSize: TcxDBMaskEdit;
    EDTrebFolio: TcxDBMaskEdit;
    EDTrebReels: TcxDBMaskEdit;
    BVL1: TsBevel;
    LBL4: TsLabel;
    ICBOmode: TcxDBImageComboBox;
    procedure CDStableNewRecord(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure m_InsertNewRecord;      
    procedure SetForDOCADDR;
  end;

var
  FRAMEcustBia: TFRAMEcustBia;

implementation

uses kneTypes, kneFREditSOA, kneUtils, DMskin;

{$R *.dfm}

{ TFRAMEcustBia }

constructor TFRAMEcustBia.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode';
  DataPacketName := 'CustomerBia';
  PropertyName := 'customerBia';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
//  FRAMEstatusInfo1.DataSource := DStable;  
  EDTrebAll.Style.StyleController :=  DMODskin.cxEditStyles1;
  ICBOmode.Style.StyleController :=  DMODskin.cxEditStyles1;
  EDTrebCutSize.Style.StyleController :=  DMODskin.cxEditStyles1;
  EDTrebFolio.Style.StyleController :=  DMODskin.cxEditStyles1;
  EDTrebReels.Style.StyleController :=  DMODskin.cxEditStyles1;
end;

procedure TFRAMEcustBia.CDStableNewRecord(DataSet: TDataSet);
var
  lv_Year, lv_Month, lv_Day: Word;
  lv_YearMonth: string;
begin
  inherited;

  // Aplica os valores por default definidos na metadata
  TkneDB.InitializeFieldDefaults(CDStable);

  DecodeDate(Now, lv_Year, lv_Month, lv_Day);

  lv_YearMonth := IntToStr(lv_Year) + FormatFloat('0#',lv_Month);//IntToStr(lv_Month);

```

#### **FRcustBia.dfm**

```
inherited FRAMEcustBia: TFRAMEcustBia
  Width = 791
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object LBLInicialPeriod: TsLabel [0]
    Left = 8
    Top = 16
    Width = 41
    Height = 13
    Caption = 'Period:'
  end
  object LBLEndPeriod: TsLabel [1]
    Left = 166
    Top = 16
    Width = 19
    Height = 13
    Caption = 'To:'
  end
  object Label1: TsLabel [2]
    Left = 8
    Top = 39
    Width = 72
    Height = 13
    Caption = 'Stock Sales:'
    FocusControl = EDTstockSalesPerc
  end
  object Label2: TsLabel [3]
    Left = 166
    Top = 39
    Width = 74
    Height = 13
    Caption = 'Direct Sales:'
    FocusControl = EDTdirectSalesPerc
  end
  object Label3: TsLabel [4]
    Left = 326
    Top = 39
    Width = 61
    Height = 13
    Caption = 'Ref. Days:'
    FocusControl = EDTreferenceDays
  end
  object Label4: TsLabel [5]
    Left = 476
    Top = 39
    Width = 94
    Height = 13
    Caption = 'Tolerance Days:'
    FocusControl = EDTreferenceDays
  end
  object LBLrebate: TsLabel [6]
    Left = 8
    Top = 65
    Width = 46
    Height = 13
    Caption = 'Rebates'
  end
  object LBLall: TsLabel [7]
    Left = 42
    Top = 117
    Width = 19
    Height = 13
    Caption = 'All:'
  end
  object LBLcutSize: TsLabel [8]
    Left = 168
    Top = 117
    Width = 49
    Height = 13
    Caption = 'CutSize:'
  end
  object LBLfolio: TsLabel [9]
    Left = 294
    Top = 117
    Width = 31
    Height = 13
    Caption = 'Folio:'
  end
  object LBLreels: TsLabel [10]
    Left = 400
    Top = 117
    Width = 36
    Height = 13
    Caption = 'Reels:'
  end
  object BVL1: TsBevel [11]
    Left = 71
    Top = 72
    Width = 565
    Height = 3
    Shape = bsTopLine
  end
  object LBL4: TsLabel [12]
    Left = 42
    Top = 91
    Width = 35
    Height = 13
    Caption = 'Mode:'
  end
  inherited PNLfooter: TsPanel
```
<!-- tabs:end -->

