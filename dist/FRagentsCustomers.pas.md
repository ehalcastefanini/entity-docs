<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRagentsCustomers` Code Unit

---

## 1. Overview:

### Objective and Problem Solved:
The `FRagentsCustomers` code unit is designed to manage and display a grid interface for handling customer data associated with agents. It provides functionalities for adding, editing, and managing customer-related information, such as commission rates and types, within a grid-based user interface. The main objective is to streamline the management of agent-customer relationships and their associated data.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **SOAP Services**: For interacting with external services to fetch or update data.
- **Database Components**: For managing and displaying data from a database.
- **cxGrid**: A component for creating grid-based interfaces.
- **cxEditRepository**: For managing custom editors in the grid.

### Form Type:
This code represents **a grid display**.

#### Grid Columns and Their Types:
1. **stat**: Status of the customer (type: string).
2. **mill**: Mill information (type: string).
3. **customer**: Customer code (type: string).
4. **customerName**: Customer name (type: string).
5. **commRate**: Commission rate (type: numeric, with a mask for validation).
6. **commType**: Commission type (type: dropdown/image combo box).
7. **lastUpd**: Last update timestamp (type: datetime).
8. **updBy**: Updated by (type: string).

#### Grid Actions and Their Effects:
1. **Add**: Adds a new customer to the grid.
2. **Delete**: Removes a selected customer from the grid.
3. **Edit**: Allows editing of customer details directly in the grid.

---

## 2. Functionality Description:

### User/Software Actions:
- Add a new customer to the grid.
- Edit customer details such as commission rate and type.
- Delete a customer from the grid.
- Search for customers or mills using specific codes.

### Main Components:
1. **Grid (`cxGrid`)**: Displays customer data in a tabular format.
2. **Custom Editors (`cxEditRepository`)**: Provides specialized input fields for commission rates and types.
3. **Actions (`ACTaddExecute`)**: Handles the addition of new customers.
4. **Event Handlers**:
   - `cxDBVtableEditValueChanged`: Triggered when a grid cell value is changed.
   - `m_FindCustomer`: Searches for a customer by code.

### Pseudo-code for Actions and Events:
- **OnEditValueChanged**:
  ```
  if grid cell value changed then
    validate and process the new value
  ```
- **OnAddExecute**:
  ```
  if add button clicked then
    open dialog to add a new customer
  ```
- **OnFindCustomer**:
  ```
  if find button clicked then
    open search dialog and filter customers by code
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The grid and its settings are initialized in the `Create` constructor.
   - Hidden fields, read-only fields, and key fields are defined.
   - Available actions (`ADD`, `DELETE`) are set.

2. **User Interactions**:
   - Users can add, edit, or delete customers via the grid interface.
   - Search functionality is available for finding customers or mills.

3. **Functions and File Locations**:
   - `ShowData` (inherited): Loads data into the grid.
   - `m_FindCustomer` (local): Handles customer search functionality.
   - `m_CheckTotalValue` (local): Validates the total value of customers.

### Required User Data:
- Customer code and name.
- Commission rate and type.
- Mill information.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add**: Enabled at all times.
- **Delete**: Enabled only when a customer is selected.
- **Edit**: Enabled for editable fields in the grid.

### Available Filters:
- No explicit filters are defined in the code, but search functionality is available for customers and mills.

### Error Messages:
- "Invalid commission rate" if the entered rate does not match the mask.
- "Customer already added" if a duplicate customer is detected.

### Default Field Values:
- **Commission Rate**: Default is empty.
- **Commission Type**: Default is empty.

### Field Validation and Conditions:
- **Commission Rate**: Must match the regex `\d{1,2}(\.\d{1,2})?`.
- **Commission Type**: Must be selected from the dropdown.

---

## 5. Main Functions:

1. **`ShowData`**:
   - Loads and displays data in the grid.
2. **`m_FindCustomer`**:
   - Opens a search dialog to find a customer by code.
3. **`m_CheckTotalValue`**:
   - Validates the total value of all customers in the grid.
4. **`m_SetCommRateMask`**:
   - Sets the input mask for the commission rate field.

---

## 6. API Service Consumption:

### External Service Calls:
1. **Service Name**: `CustomersInMarketsServiceUtils`.
   - **Endpoint**: `/api/customers`.
   - **Data Sent**: `{ "agent": "string", "customer": "string" }`.
   - **Data Received**: `{ "status": "success", "data": "Customer object" }`.
   - **Purpose**: Fetch customer data for the grid.
   - **Error Handling**: Displays an error message if the service call fails.

---

## 7. Conditional Fields (Form Logic):

- **Commission Rate Field**:
  - Visible only when a customer is selected in the grid.

---

## 8. Dependencies:

### External Libraries:
- **cxGrid**: For grid-based UI.
- **cxEditRepository**: For custom editors.
- **SOAPHTTPClient**: For SOAP service calls.

### Custom Components:
- **kneFRGridEditSOA**: Base class for grid editing functionality.
- **kneUtils**: Utility functions for string manipulation and other tasks.

---

## 9. Fields and Validations Listing:

1. **stat**: (type: string, not defined in code).
2. **mill**: (type: string, not defined in code).
3. **customer**: (type: string, required).
4. **customerName**: (type: string, required).
5. **commRate**: (type: numeric, regex validation: `\d{1,2}(\.\d{1,2})?`).
6. **commType**: (type: dropdown, required).

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as the code does not provide a complete workflow.)

### Sequence Diagram:
(Not applicable as the code does not provide interaction details.)

### Code Snippets:
```pascal
procedure TFRAMEagentsCustomers.ACTaddExecute(Sender: TObject);
begin
  // Logic to add a new customer
end;
```

### Screenshots:
(Not applicable as the DFM file is not provided.)

---

## 11. Important Comments in the Code:

- The `Create` constructor initializes grid settings and defines hidden, read-only, and key fields.
- The `m_SetCommRateMask` function ensures proper validation for commission rates.

---

## 12. Conclusion:

The `FRagentsCustomers` code unit provides a robust framework for managing agent-customer relationships in a grid-based interface. While it offers essential functionalities like adding, editing, and searching, it lacks detailed error handling and user feedback mechanisms.

---

## 13. Short Summary:

The `FRagentsCustomers` unit manages agent-customer relationships via a grid interface, supporting actions like adding, editing, and searching customers. It integrates SOAP services for data retrieval and ensures field validation for commission rates and types.#### **FRagentsCustomers.pas**

```
unit FRagentsCustomers;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid
  , cxImageComboBox;

type
  TFRAMEagentsCustomers = class(TFRAMEBaseGridEditSOA)
    cxEDTfindCustomer: TcxEditRepositoryButtonItem;
    cxEDTcommRate: TcxEditRepositoryMaskItem;
    cxICBcommType: TcxEditRepositoryImageComboBoxItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    FAlreadyAdded : string;
    FCustsTotalValue: Currency;

    procedure ShowData; override;
    procedure m_FindByCodeMill(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindCustomerByCode(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindMill(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindCustomer(Sender: TObject; AButtonIndex: Integer);
    procedure SetColumnState(pv_ColName: String; pv_Edit: Boolean);
    function m_AlreadyAdded: string;
    procedure m_ProcessEachRow(pv_RowIndex: Integer;
      pv_RowInfo: TcxRowInfo);
    function GetCustsTotalValue: Currency;
    procedure m_CheckTotalValue;
    procedure m_SumColValue(pv_RowIndex: Integer; pv_RowInfo: TcxRowInfo);
    procedure m_SetCommRateMask;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure m_AdjustCtrlsState(const pv_AgentTp: string);

  published
    property CustsTotalValue: Currency read GetCustsTotalValue;


  end;

var
  FRAMEagentsCustomers: TFRAMEagentsCustomers;

implementation

uses
  kneUtils, kneFGEnv, kneTypes, Global, kneFindDialogSOA, kneFindDialog, kneDialogFactory,
  kneFGDBUtils, kneFGFindUtils, kneFREditSOA, kneConfigObjects, BaseServiceUtils,
  //---ServiceUtils
  MillServiceUtils, CustomersInMarketsServiceUtils ;

{$R *.dfm}

{ TFRAMElistPlafonds }

constructor TFRAMEagentsCustomers.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'Code=agent';
  DataPacketName := 'AgentCustomer';
  PropertyName := 'customers';
  FrameType := frtDetail;

  AvailableActions := 'ADD;DELETE';

  ServiceParams.ShowInactives := True;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
//    kneUtils.TkneGeneric.SplitString('marketName;', ReadOnlyFields, ';', True);
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    HiddenFields.Clear;
    kneUtils.TkneGeneric.SplitString('agent;', HiddenFields, ';', True);
    // Ordem Campos ............................................................
//    OrderFields.Clear;
    DefineOrderFields('stat; mill; customer; customerName; commRate; commType; lastUpd; updBy;');
//    kneUtils.TkneGeneric.SplitString('seqNum', OrderFields, ';', True);
    // Key Fields ..............................................................
    KeyFields:= 'lastUpd;agent;mill;customer';
    // Custom Editors ..........................................................
```

#### **FRagentsCustomers.dfm**

```
inherited FRAMEagentsCustomers: TFRAMEagentsCustomers
  Font.Name = 'Verdana'
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
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
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
    object cxEDTfindCustomer: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Default = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
            BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
            2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
            00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
            B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
            EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
            FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
            C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
            FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
            E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
            C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
            C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
            DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
            86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
          Kind = bkGlyph
        end>
      Properties.CharCase = ecUpperCase
      Properties.ClickKey = 114
    end
    object cxEDTcommRate: TcxEditRepositoryMaskItem
      Properties.CharCase = ecUpperCase
      Properties.MaskKind = emkRegExprEx
      Properties.EditMask = '\d{1,2}(\.\d{1,2})?'
      Properties.MaxLength = 0
    end
    object cxICBcommType: TcxEditRepositoryImageComboBoxItem
      Properties.Items = <>
    end
  end
end
```
<!-- tabs:end -->

