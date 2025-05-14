<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustProdMill` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRcustProdMill` code unit defines a Delphi frame (`TFRAMEcustProdMill`) that extends a base grid-editing frame (`TFRAMEBaseGridEditSOA`). Its primary purpose is to manage and display a grid of customer-mill relationships, allowing users to view and interact with data related to business units, mill codes, descriptions, and SAP information. The frame provides functionality for configuring grid properties, managing column states, and handling specific business logic for customer-mill data.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the user interface and managing components.
- **DevExpress cxGrid**: A third-party grid component for displaying and interacting with tabular data.
- **SOAP Services**: Used for interacting with external services (e.g., `MillServiceUtils`).
- **Custom Components**: Includes custom styles, panels, and edit repositories.

### Form Type:
This code represents a **grid display**.

#### Grid Columns and Their Types:
1. **checked**: Boolean (checkbox).
2. **businessUnit**: String.
3. **millCode**: String.
4. **millDesc**: String.
5. **millCustCode**: String.
6. **sap**: String.

#### Grid Actions and Their Effects:
- **Read-Only Fields**: Certain fields are marked as non-editable.
- **Hidden Fields**: Fields can be hidden from the grid.
- **Custom Editors**: Specific columns (e.g., `checked`) use custom editors like checkboxes.
- **Key Fields**: Defines primary keys for the grid.

---

## 2. Functionality Description:

### User/Software Actions:
1. **View Customer-Mill Data**: Displays a grid with customer-mill relationships.
2. **Set Column States**: Allows enabling or disabling editing for specific columns.
3. **Configure Grid Properties**: Customizes grid settings such as read-only fields, hidden fields, and column order.
4. **Set for Document Address (DOCADDR)**: Adjusts the grid for specific use cases by disabling editing and hiding the footer panel.

### Main Components:
- **Grid (`cxGrid`)**: Displays customer-mill data.
- **Footer Panel (`PNLfooter`)**: Provides additional actions (hidden in some cases).
- **Custom Checkbox Editor (`cxEDTchecked`)**: Used for the `checked` column.

### Pseudo-Code for Actions and Events:
- **OnCreate Event**:
  ```
  if frame is created then
    initialize grid settings
    set default properties
  ```
- **Set Column State**:
  ```
  if column state needs to be changed then
    set column editable or non-editable
  ```
- **Set for DOCADDR**:
  ```
  if SetForDOCADDR is called then
    disable editing for all columns
    hide footer panel
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created using the `Create` constructor.
   - Grid settings are configured (e.g., read-only fields, hidden fields, column order).
   - Default properties like `MasterKeyFields`, `DataPacketName`, and `FrameType` are set.

2. **User Interactions**:
   - Users interact with the grid to view or modify data (if allowed).
   - Specific actions like `SetForDOCADDR` or `SetKeyEditing` can be triggered programmatically.

### Functions and File Locations:
- **`Create`** (Initialization): `FRcustProdMill.pas`.
- **`SetColumnState`** (Column State Management): `FRcustProdMill.pas`.
- **`SetForDOCADDR`** (Adjust Grid for DOCADDR): `FRcustProdMill.pas`.
- **`SetKeyEditing`** (Key Editing Management): `FRcustProdMill.pas`.

### Required Data:
- Customer-mill data, including `businessUnit`, `millCode`, `millDesc`, `millCustCode`, and `sap`.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Set Column State**: Requires the column name and edit state as inputs.
- **Set for DOCADDR**: Disables editing for all columns and hides the footer panel.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- **`checked`**: Default value is `0` (unchecked).

### Field Validation and Conditions:
- **`checked`**: Boolean values (`1` for checked, `0` for unchecked).
- Other field validations are not explicitly defined in the code.

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame and configures grid settings.
   - Sets default properties like `MasterKeyFields` and `DataPacketName`.

2. **`SetColumnState`**:
   - Enables or disables editing for a specific column.

3. **`SetForDOCADDR`**:
   - Disables editing for all columns and hides the footer panel.

4. **`SetKeyEditing`**:
   - Disables editing for specific key columns.

---

## 6. API Service Consumption:

- **Service Name**: `MillServiceUtils`.
- **Purpose**: Interacts with mill-related data (details not explicitly defined in the code).

---

## 7. Conditional Fields (Form Logic):

- **Footer Panel (`PNLfooter`)**:
  - Visible only when not in DOCADDR mode.

---

## 8. Dependencies:

### External Libraries:
- **DevExpress cxGrid**: Used for grid display and interaction.
- **SOAPHTTPClient**: Used for SOAP service communication.

### Custom Components:
- **`cxEDTchecked`**: Custom checkbox editor for the `checked` column.

---

## 9. Fields and Validations Listing:

1. **checked**: Boolean, default `0` (unchecked).
2. **businessUnit**: String, no validation defined.
3. **millCode**: String, no validation defined.
4. **millDesc**: String, no validation defined.
5. **millCustCode**: String, no validation defined.
6. **sap**: String, no validation defined.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Frame Created] --> [Initialize Grid Settings] --> [User Interacts with Grid]
```

### Sequence Diagram:
```plaintext
User --> Frame: View/Modify Data
Frame --> Grid: Display Data
```

### Code Snippets:
```delphi
procedure TFRAMEcustProdMill.SetColumnState(pv_ColName: String; pv_Edit: Boolean);
begin
  cxDBVtable.Columns[pv_ColName].Options.Editing := pv_Edit;
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **`[18-03-2016, #22748]`**: Indicates a specific change or feature addition.
- **`[14-10-2015, #22272]`**: Documents the order of fields in the grid.

---

## 12. Conclusion:

The `FRcustProdMill` code unit provides a robust framework for managing customer-mill data in a grid format. It offers flexibility in configuring grid properties and supports specific business logic. However, the lack of explicit error handling and field validations may limit its robustness in certain scenarios.

---

## 13. Short Summary:

The `FRcustProdMill` unit defines a grid-based frame for managing customer-mill relationships, offering configurable grid settings and specific business logic for data interaction. It integrates with SOAP services and uses DevExpress components for enhanced UI functionality.#### **FRcustProdMill.pas**

```
unit FRcustProdMill;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel;

type
  TFRAMEcustProdMill = class(TFRAMEBaseGridEditSOA)
    cxEDTchecked: TcxEditRepositoryCheckBoxItem;
  private
   { Private declarations }
    procedure SetColumnState(pv_ColName: String; pv_Edit: Boolean);
    function GetSelectedBusUnits: string;
   public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure SetKeyEditing(const EditKey: Boolean); override;
    procedure SetForDOCADDR;
    procedure clearCustMillSap;

   published
     property SelectedBusUnits: string read GetSelectedBusUnits;   // [18-03-2016, #22748]
  end;

var
  FRAMEcustProdMill: TFRAMEcustProdMill;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, BaseServiceUtils, kneTypes, Global,
  //---
  MillServiceUtils
  , kneFREditSOA;

{$R *.dfm}

{ TFRAMEcustProdMill }

constructor TFRAMEcustProdMill.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode';
  DataPacketName := 'CustomerMill';
  PropertyName := 'customerMill';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    DefineReadOnlyFields('businessUnit; millCode; millDesc; millCustCode; sap');
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('checked; businessUnit; millCode; millDesc; millCustCode; sap');  // [14-10-2015, #22272]
    // Key Fields ..............................................................
    KeyFields:= 'customerCode;millCode;agentCode';
    // Custom Editors ..........................................................
    AddCustomField('checked','cxEDTchecked');
  end; //with
  ColsWidthInGrid := '75;50;50;350;100';

end;

procedure TFRAMEcustProdMill.SetForDOCADDR;
var
  lv_i : Integer;
begin
  for lv_i := 0 to cxDBVtable.ColumnCount - 1 do
     cxDBVtable.Columns[lv_i].Options.Editing := False;

  PNLfooter.Visible := False;
end;

procedure TFRAMEcustProdMill.SetKeyEditing(const EditKey: Boolean); 
begin
  inherited;
  SetColumnState('millCode', False);
end; 

// define o estado de uma coluna da grid
procedure TFRAMEcustProdMill.SetColumnState(pv_ColName: String;
  pv_Edit: Boolean);
```

#### **FRcustProdMill.dfm**

```
inherited FRAMEcustProdMill: TFRAMEcustProdMill
  ParentFont = True
  inherited PNLfooter: TsPanel
    Visible = False
  end
  inherited cxSTLR: TcxStyleRepository
    inherited cxSTLReadOnly: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLDefault: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLInactive: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLgroupBox: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLheader: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLselection: TcxStyle
      Font.Name = 'Verdana'
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTchecked: TcxEditRepositoryCheckBoxItem
      Properties.ValueChecked = '1'
      Properties.ValueUnchecked = '0'
    end
  end
end
```
<!-- tabs:end -->

