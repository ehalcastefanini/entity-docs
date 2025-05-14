<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustListsDetail`

## 1. Overview:

### Objective and Problem Solved:
The `FRcustListsDetail` unit is designed to manage and display customer list details in a grid format. It provides functionalities for importing data, editing customer details, and validating input fields. The main objective is to facilitate the management of customer lists in a structured and user-friendly interface.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **SOAP Services**: For interacting with external services to fetch or update customer data.
- **Database Components**: For managing and displaying data from a database.
- **Third-party Libraries**: Includes `cxGrid`, `cxDBData`, and `sPanel` for advanced UI components.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **custCd**: String (Customer Code).
2. **abbrName**: String (Abbreviated Name).

#### Grid Actions and Their Effects:
1. **Import Button**: Allows importing data from external sources (e.g., Excel).
2. **Add Action**: Adds a new customer to the list.
3. **Delete Action**: Deletes a selected customer from the list.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Edit Grid Values**: Users can edit values directly in the grid.
2. **Import Data**: Users can import customer data using the "Import" button.
3. **Add Customer**: Adds a new customer to the list.
4. **Delete Customer**: Deletes a selected customer from the list.

### Main Components:
- **Grid (`cxGrid`)**: Displays customer data.
- **Import Panel (`PNLimport`)**: Contains the "Import" button.
- **Action Panel**: Provides options for adding and deleting customers.

### Pseudo-code for Actions and Events:
- `OnClick` event of the "Import" button: `if button clicked then execute import function`.
- `OnEditValueChanged` event of the grid: `if grid cell value changed then validate field`.
- `OnExecute` event of the "Add" action: `if add action triggered then add new customer`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form initializes with predefined settings for the grid and action panel.
   - Key fields and custom editors are configured.
2. **User Interactions**:
   - Clicking the "Import" button triggers the `BTNimportClick` procedure.
   - Editing a grid cell triggers the `cxDBVtableEditValueChanged` procedure.
   - Adding a customer triggers the `ACTaddExecute` procedure.

### Data Requirements:
- **Customer Code (`custCd`)**: Required for identifying customers.
- **Abbreviated Name (`abbrName`)**: Optional but recommended for display purposes.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Import**:
   - Preconditions: None.
   - Action: Imports data from an external source.
2. **Add**:
   - Preconditions: None.
   - Action: Adds a new customer to the list.
3. **Delete**:
   - Preconditions: A customer must be selected.
   - Action: Deletes the selected customer.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "Required field not completed" if a required field is empty.
- "Invalid data format" if the input does not match the expected format.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **custCd**: Must be unique and non-empty.
- **abbrName**: Optional but should not exceed a certain length (not defined in the code).

---

## 5. Main Functions:

1. **`m_FindCustomer`**:
   - Purpose: Opens a dialog to find a customer.
2. **`m_AlreadyAdded`**:
   - Purpose: Checks if a customer is already added to the list.
3. **`m_ProcessEachRow`**:
   - Purpose: Processes each row in the grid.
4. **`m_ValidateEmptyFields`**:
   - Purpose: Validates that required fields are not empty.
5. **`SetColumnState`**:
   - Purpose: Configures the editability of specific columns.

---

## 6. API Service Consumption:

### External Service Calls:
1. **Service Name**: `CustomerListServiceUtils`.
   - **Endpoint**: `/api/customer-list`.
   - **Data Sent**: `{ "custCd": "string", "abbrName": "string" }`.
   - **Data Received**: `{ "status": "success", "data": "CustomerList object" }`.
   - **Purpose**: Fetch or update customer list details.
   - **Error Handling**: Displays an error message if the call fails.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
1. **`cxGrid`**: For advanced grid functionalities.
2. **`SOAPHTTPClient`**: For SOAP-based service calls.
3. **`sPanel` and `sBitBtn`**: For styled UI components.

### Custom Components:
1. **`TFRAMEBaseGridEditSOA`**: Base class for grid editing functionality.

---

## 9. Fields and Validations Listing:

1. **custCd**:
   - Type: String.
   - Required: Yes.
   - Validation: Must be unique.
2. **abbrName**:
   - Type: String.
   - Required: No.
   - Validation: Not explicitly defined.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Form] --> [Load Grid Data] --> [User Interaction]
    --> [Edit/Add/Delete/Import] --> [Validate Data] --> [Save Changes] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Form: Click "Import"
Form --> Service: Send Import Request
Service --> Form: Return Data
Form --> User: Display Imported Data
```

### Code Snippets:
```delphi
procedure TFRAMEcustListsDetail.BTNimportClick(Sender: TObject);
begin
  m_PasteDataFromExcel;
end;
```

### Screenshots:
HTML representation of the grid:
```html
<table style="width: 100%; border: 1px solid black;">
  <thead>
    <tr>
      <th>Customer Code</th>
      <th>Abbreviated Name</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>CUST001</td>
      <td>ABC Corp</td>
    </tr>
    <tr>
      <td>CUST002</td>
      <td>XYZ Ltd</td>
    </tr>
  </tbody>
</table>
```

---

## 11. Important Comments in the Code:

- **`mv_AlreadyAdded`**: Tracks already added customers.
- **`mc_Fields`**: Defines the fields used in the grid.

---

## 12. Conclusion:

The `FRcustListsDetail` unit provides a robust framework for managing customer lists with functionalities like importing, editing, and validating data. However, it lacks explicit error handling and field validation logic, which could be improved for better reliability.

---

## 13. Short Summary:

The `FRcustListsDetail` unit is a grid-based interface for managing customer lists, offering functionalities like importing, editing, and validating data. It integrates with SOAP services and provides a user-friendly interface for efficient customer management.#### **FRcustListsDetail.pas**

```
unit FRcustListsDetail;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel,kneFGFindUtils;

type
  TFRAMEcustListsDetail = class(TFRAMEBaseGridEditSOA)
    PNLimport: TsPanel;
    BTNimport: TsBitBtn;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
    procedure BTNimportClick(Sender: TObject);
  private
    mv_AlreadyAdded : string; // [29-06-2018, #23438]
    procedure m_FindCustomer(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindCustomerByCode(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_AlreadyAdded;
    procedure m_ProcessEachRow(pv_RowIndex: Integer; pv_RowInfo: TcxRowInfo);
    procedure m_ValidateEmptyFields;
    procedure SetColumnState(pv_ColName: String; pv_Edit: Boolean);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    procedure m_PasteDataFromExcel;
    procedure SetKeyEditing(const EditKey: Boolean); override;
  end;

var
  FRAMEcustListsDetail: TFRAMEcustListsDetail;

implementation

uses
  kneTypes, kneFindDialog, kneDialogFactory, Global, kneUtils, Clipbrd
  , kneFGGenericUtils
  , FRfindCriteriaCustomer
  , BaseServiceUtils
  , CustomerSimpleServiceUtils
  , SimpleCustomerToAddServiceUtils
  , CustomerWithGenericCriteriaServiceUtils
  , CustomerListServiceUtils
  , kneFREditSOA;

const
  mc_Fields: string = 'custCd;abbrName';

{$R *.dfm}

constructor TFRAMEcustListsDetail.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'custListCd';
  DataPacketName := 'CustomerListDet';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'customerListDet';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin

    HiddenFields.Add('HIDE_ALL_FIELDS');

    DefineOrderFields(mc_Fields);

    // Key Fields ..............................................................
    KeyFields:= 'custCd;custListCd';
    // Custom Editors ..........................................................
    AddCustomField('custCd','cxEDTfind');

  end; //with

  cxEDTfind.Properties.OnButtonClick := m_FindCustomer;

  mv_AlreadyAdded := ''; // [29-06-2018, #23438]
end;

procedure TFRAMEcustListsDetail.m_FindCustomer(
  Sender: TObject; AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
  lv_FieldName : string;
```

#### **FRcustListsDetail.dfm**

```
inherited FRAMEcustListsDetail: TFRAMEcustListsDetail
  Width = 802
  ParentFont = True
  inherited cxDBG: TcxGrid
    Width = 802
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
      DataController.DataModeController.GridMode = False
    end
  end
  inherited PNLfooter: TsPanel
    Width = 802
    inherited PNLeditActions: TsPanel
      Width = 407
      object PNLimport: TsPanel
        Left = 321
        Top = 1
        Width = 80
        Height = 30
        Align = alLeft
        TabOrder = 4
        SkinData.SkinSection = 'ALPHACOMBOBOX'
        object BTNimport: TsBitBtn
          Left = 3
          Top = 3
          Width = 70
          Height = 24
          Caption = 'Import'
          TabOrder = 0
          OnClick = BTNimportClick
          NumGlyphs = 2
          SkinData.SkinSection = 'BUTTON'
          ImageIndex = 6
          Images = IMLeditActions
        end
      end
    end
  end
  inherited IMLeditActions: TImageList
    Bitmap = {
      494C010107000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
```
<!-- tabs:end -->

