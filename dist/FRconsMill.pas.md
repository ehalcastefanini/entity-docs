<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRconsMill` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRconsMill` code unit defines a Delphi frame (`TFRAMEconsMill`) that extends a base grid-editing frame (`TFRAMEBaseGridEditSOA`). Its primary purpose is to manage and display a grid of data related to "Consignee Mills." It provides functionality for configuring grid columns, handling user interactions, and managing data visibility and editing states. This frame is particularly useful in applications where users need to view, edit, or manage data in a tabular format.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **cxGrid**: A component for displaying and managing tabular data.
- **SOAPHTTPClient**: For potential SOAP-based web service communication.
- **DBClient**: For managing database connections and datasets.
- **Custom Components**: Includes `kneFRGridEditSOA`, `kneFRGridManager`, and others for specialized functionality.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **checked**: Checkbox (custom editor).
2. **millCode**: String (read-only).
3. **millDesc**: String.
4. **millConsCode**: String.

#### Grid Actions and Their Effects:
- **SetColumnState**: Toggles the editability of a specific column.
- **SetKeyEditing**: Disables editing for the `millCode` column.
- **Custom Field Editors**: Adds a checkbox editor for the `checked` column.

---

## 2. Functionality Description:

### User/Software Actions:
1. **View Data**: Displays a grid of "Consignee Mills" with specific columns.
2. **Edit Data**: Allows editing of certain columns based on predefined rules.
3. **Configure Grid**: Automatically sets up grid properties like hidden fields, column order, and custom editors.

### Main Components:
- **Grid Settings**: Configures the grid's behavior, such as hidden fields, column order, and key fields.
- **Custom Editors**: Adds a checkbox editor for the `checked` column.
- **Column State Management**: Controls whether a column is editable or read-only.

### Pseudo-Code for Actions and Events:
- **On Frame Initialization**:
  ```
  if frame is created then
    set MasterKeyFields, DataPacketName, PropertyName, and FrameType
    configure grid settings (hidden fields, column order, key fields, custom editors)
    set default column widths
  ```
- **SetKeyEditing**:
  ```
  if SetKeyEditing is called then
    disable editing for 'millCode' column
  ```
- **SetColumnState**:
  ```
  if SetColumnState is called with column name and edit state then
    find the column by name
    if column exists then
      set column's editing state
      apply appropriate style (editable or read-only)
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created using the `Create` constructor.
   - Grid settings are configured, including hidden fields, column order, and custom editors.
   - Default column widths are set.

2. **User Interaction**:
   - Users interact with the grid to view or edit data.
   - Editing is restricted based on column-specific rules.

3. **Functions and File Locations**:
   - `Create` (Initialization): Defined in `FRconsMill.pas`.
   - `SetKeyEditing` (Disable Editing for Key Fields): Defined in `FRconsMill.pas`.
   - `SetColumnState` (Manage Column Editability): Defined in `FRconsMill.pas`.

### Required User Data:
- Users interact with the grid to view or edit data. No additional input is required beyond interacting with the grid.

---

## 4. Business Rules:

### Actions and Preconditions:
- **SetKeyEditing**: Disables editing for the `millCode` column. No preconditions required.
- **SetColumnState**: Requires a valid column name to toggle its editability.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- **checked**: Default value is `0` (unchecked).

### Field Validation and Conditions:
- **checked**: Valid values are `1` (checked) or `0` (unchecked).
- Other field validations are not explicitly defined in the code.

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame and configures grid settings.
   - Sets default properties like `MasterKeyFields`, `DataPacketName`, and `FrameType`.

2. **`SetKeyEditing`**:
   - Disables editing for the `millCode` column.

3. **`SetColumnState`**:
   - Toggles the editability of a specific column and applies appropriate styles.

---

## 6. API Service Consumption:

- No external API calls are explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **cxGrid**: For grid display and management.
- **SOAPHTTPClient**: For potential SOAP-based web service communication.
- **DBClient**: For managing database connections and datasets.

### Custom Components:
- **kneFRGridEditSOA**: Base class for grid-editing frames.
- **kneFRGridManager**: Manages grid settings and behavior.

---

## 9. Fields and Validations Listing:

1. **checked**:
   - Type: Checkbox.
   - Default: `0` (unchecked).
   - Valid Values: `1` (checked), `0` (unchecked).

2. **millCode**:
   - Type: String.
   - Read-Only.

3. **millDesc**:
   - Type: String.

4. **millConsCode**:
   - Type: String.

- Field constraints and validations are not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Frame Initialization] --> [Grid Configuration] --> [User Interaction] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Frame: Interact with grid
Frame --> Grid: Display data
Grid --> Frame: Handle user actions
```

### Code Snippets:
```delphi
procedure TFRAMEconsMill.SetKeyEditing(const EditKey: Boolean);
begin
  inherited;
  SetColumnState('millCode', False);
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **Grid Settings**:
  - Configures hidden fields, column order, and custom editors.
- **SetColumnState**:
  - Manages column editability and applies appropriate styles.

---

## 12. Conclusion:

The `FRconsMill` code unit provides a robust framework for managing and displaying tabular data related to "Consignee Mills." Its strengths include configurable grid settings and column-specific editing rules. However, it lacks explicit error handling and field validation, which could be improved for better user experience.

---

## 13. Short Summary:

The `FRconsMill` unit defines a grid-based frame for managing "Consignee Mills" data. It supports configurable grid settings, column-specific editing rules, and custom field editors, making it suitable for applications requiring tabular data management.#### **FRconsMill.pas**

```
unit FRconsMill;

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
  TFRAMEconsMill = class(TFRAMEBaseGridEditSOA)
    cxEDTchecked: TcxEditRepositoryCheckBoxItem;
  private
    { Private declarations }
    procedure SetColumnState(pv_ColName: String; pv_Edit: Boolean);
    function GetSelectedBusUnits: string;
   public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure SetKeyEditing(const EditKey: Boolean); override;

   published
     property SelectedBusUnits: string read GetSelectedBusUnits;

  end;

var
  FRAMEconsMill: TFRAMEconsMill;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, 
  kneTypes;

{$R *.dfm}

{ TFRAMEconsMill }

constructor TFRAMEconsMill.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'consCode=consigneeCode';
  DataPacketName := 'ConsigneeMill';
  PropertyName := 'consigneeMill';
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
    // Campos Hidden ...........................................................
    DefineHiddenFields('consigneeCode');
    // Ordem Campos ............................................................
    DefineOrderFields('checked; millCode; millDesc; millConsCode');
    // Key Fields ..............................................................
    KeyFields:= 'consigneeCode;millCode';
    // Custom Editors ..........................................................
    AddCustomField('checked','cxEDTchecked');
  end; //with
  ColsWidthInGrid := '80;50;350;100';
  //@@@@@
  CDStable.Tag := 3;
  DStable.Tag := 3;
end;

procedure TFRAMEconsMill.SetKeyEditing(const EditKey: Boolean); 
begin
  inherited;
  SetColumnState('millCode', False);
end; 

// define o estado de uma coluna da grid
procedure TFRAMEconsMill.SetColumnState(pv_ColName: String;
  pv_Edit: Boolean);
var
  lv_column : TcxCustomGridTableItem;
begin
  try
    lv_column := cxDBVtable.GetColumnByFieldName(pv_ColName);
    if Assigned(lv_column) then                     // se encontrou a coluna
    begin                                           // Item normal
      lv_column.Options.Editing := pv_Edit;         // define o estado de edi��o da coluna
      if pv_Edit then
        lv_column.Styles.Content := cxSTLDefault
      else
        lv_column.Styles.Content := cxSTLReadOnly;
```

#### **FRconsMill.dfm**

```
inherited FRAMEconsMill: TFRAMEconsMill
  inherited PNLfooter: TsPanel
    Visible = False
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

