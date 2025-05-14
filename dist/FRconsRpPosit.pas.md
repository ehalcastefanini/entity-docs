<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRconsRpPosit` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRconsRpPosit` code unit defines a frame (`TFRAMEconsRpPosit`) that provides a grid-based interface for managing and editing data related to "Consignee Rp Positions." It allows users to view, add, and delete records, as well as perform specific actions like searching and filtering data. The main objective is to provide a user-friendly interface for managing business unit and position-related data.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **cxGrid**: A component for displaying and managing tabular data.
- **SOAP Services**: For interacting with external services to fetch or update data.
- **Database Components**: For managing and interacting with datasets.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **businessUnit**: ComboBox (Dropdown list).
2. **rpPositCode**: Custom field with a search button.
3. **rpPosit**: Text field.

#### Grid Actions and Their Effects:
1. **Add**: Adds a new record to the grid.
2. **Delete**: Deletes the selected record from the grid.
3. **Edit**: Allows editing of existing records in the grid.
4. **Search**: Provides functionality to search for specific records.

---

## 2. Functionality Description:

### User/Software Actions:
1. Add a new record.
2. Delete an existing record.
3. Edit a record's values.
4. Search for records using specific criteria.
5. Populate the "Business Unit" dropdown with data from an external service.

### Main Components:
- **Grid (`cxGrid`)**: Displays the data in a tabular format.
- **ComboBox (`cxCBXbusUnit`)**: Dropdown for selecting a business unit.
- **Search Button (`cxEDTfind`)**: Allows users to search for specific records.
- **Action Panel**: Provides buttons for adding and deleting records.

### Pseudo-code for Actions and Events:
- `OnEditValueChanged` event of the grid:
  ```
  if grid cell value changed then
    update the corresponding dataset field
  ```
- `OnButtonClick` event of the search button:
  ```
  if search button clicked then
    open search dialog and fetch selected value
  ```
- `OnExecute` event of the Add action:
  ```
  if Add button clicked then
    create a new record in the dataset
  ```
- `AfterPost` event of the dataset:
  ```
  if record saved then
    refresh the grid
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized with default settings, including grid configuration and event assignments.
   - The "Business Unit" dropdown is populated using the `m_GetBusinessUnit` method.

2. **User Interactions**:
   - Users can add, delete, or edit records in the grid.
   - Users can search for specific records using the search button.

3. **Functions and File Locations**:
   - `Create` (File: `FRconsRpPosit.pas`): Initializes the frame and sets up the grid.
   - `m_GetBusinessUnit` (File: `FRconsRpPosit.pas`): Fetches and populates the "Business Unit" dropdown.
   - `m_GenericFind` (File: `FRconsRpPosit.pas`): Handles the search functionality.

### Required Data:
- Business Unit data for the dropdown.
- Existing records for display in the grid.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add**: Enabled at all times.
- **Delete**: Enabled only when a record is selected.
- **Edit**: Enabled only when a record is selected.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **rpPositCode**: Must be a valid code (validated via search functionality).
- **businessUnit**: Must be selected from the dropdown.

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame and configures the grid.
   - Populates the "Business Unit" dropdown.

2. **`m_GetBusinessUnit`**:
   - Fetches data for the "Business Unit" dropdown from an external service.

3. **`m_GenericFind`**:
   - Opens a search dialog and fetches the selected value.

4. **`SetKeyEditing`**:
   - Enables or disables editing of key fields.

---

## 6. API Service Consumption:

### External Service Calls:
1. **Service Name**: `LoadUnitsPosInstServiceUtils`
   - **Purpose**: Fetch data for the "Business Unit" dropdown.
   - **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- The "Business Unit" dropdown is populated dynamically using the `m_GetBusinessUnit` method.

---

## 8. Dependencies:

### External Libraries:
- **cxGrid**: For grid display and management.
- **SOAPHTTPClient**: For interacting with SOAP services.

### Custom Components:
- **kneFRGridEditSOA**: Base class for the frame.
- **kneFGFindUtils**: Utility for search functionality.

---

## 9. Fields and Validations Listing:

1. **businessUnit**:
   - Type: ComboBox.
   - Required: Yes.
   - Validation: Must be selected from the dropdown.

2. **rpPositCode**:
   - Type: Custom field with search button.
   - Required: Yes.
   - Validation: Must be a valid code.

3. **rpPosit**:
   - Type: Text field.
   - Required: No.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEconsRpPosit.m_GetBusinessUnit;
begin
  // Fetch and populate the "Business Unit" dropdown
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Grid Configuration**:
  ```delphi
  DefineHiddenFields('HIDE_ALL_FIELDS');
  DefineOrderFields(mc_GRID_FIELDS);
  ```
- **Event Assignments**:
  ```delphi
  cxEDTfind.Properties.OnButtonClick := m_GenericFind;
  ```

---

## 12. Conclusion:

The `FRconsRpPosit` code unit provides a robust and user-friendly interface for managing "Consignee Rp Positions." Its strengths include dynamic grid configuration and integration with external services. However, the lack of explicit error handling and validation logic may require additional implementation.

---

## 13. Short Summary:

The `FRconsRpPosit` unit defines a grid-based interface for managing "Consignee Rp Positions," with features like adding, deleting, and searching records. It integrates with external services for data population and provides a customizable grid for efficient data management.#### **FRconsRpPosit.pas**

```
unit FRconsRpPosit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel, kneFGFindUtils, cxButtonEdit{m_GenericFind};

type
  TFRAMEconsRpPosit = class(TFRAMEBaseGridEditSOA)
    cxCBXbusUnit: TcxEditRepositoryComboBoxItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
    procedure CDStableAfterPost(DataSet: TDataSet);
    procedure CDStableAfterCancel(DataSet: TDataSet);
  private
    { Private declarations }
    procedure m_FindUnitPos(pv_FieldName : string; Sender: TObject);
    procedure m_FindByCodeUnitPos(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_GenericFind(Sender: TObject; AButtonIndex: Integer);
    procedure m_GetBusinessUnit;
    procedure m_ClearUnitPos;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure SetKeyEditing(const EditKey: Boolean);   override;
  end;

var
  FRAMEconsRpPosit: TFRAMEconsRpPosit;

implementation

uses
  kneInterfaces, kneUtils, kneFindDialogSOA, Global, kneConfigObjects,
  kneTypes,kneFindDialog,kneDialogFactory
  , LoadUnitsPosInstServiceUtils
  , BusinessUnitServiceUtils
  ;

const
  mc_GRID_FIELDS = 'businessUnit;rpPositCode;rpPosit';

{$R *.dfm}

{ TFRAMEconsRpPosit }

constructor TFRAMEconsRpPosit.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'consCode';
  DataPacketName := 'ConsigneeRpPosit';
  PropertyName := 'consigneeRpPosits';
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    DefineHiddenFields('HIDE_ALL_FIELDS');

    DefineOrderFields(mc_GRID_FIELDS);

    // Key Fields ..............................................................
    KeyFields:= 'consCode;rpPositCode';
    // Custom Editors ..........................................................
    AddCustomField('rpPositCode','cxEDTfind');
    AddCustomField('businessUnit','cxCBXbusUnit'); // [08-07-2015, #22016]
  end; //with

  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_GenericFind;  // [08-07-2015, #22016]

  m_GetBusinessUnit; // preencher CBX businessUnit
  //@@@@@
  CDStable.Tag := 8;
  DStable.Tag := 8;
end;

procedure TFRAMEconsRpPosit.m_FindByCodeUnitPos(
  Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem);
var
  lv_Service: TLoadUnitsPosInstServiceUtils;
  lv_TargetDescFields, lv_DescFields : TStringList;
  lv_Column: TcxGridDBColumn;
```

#### **FRconsRpPosit.dfm**

```
inherited FRAMEconsRpPosit: TFRAMEconsRpPosit
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
    object cxCBXbusUnit: TcxEditRepositoryComboBoxItem
      Properties.DropDownListStyle = lsFixedList
    end
  end
end
```
<!-- tabs:end -->

