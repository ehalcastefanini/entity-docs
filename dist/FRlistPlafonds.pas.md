<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRlistPlafonds` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRlistPlafonds` code unit defines a grid-based user interface for managing "Plafonds" (likely a financial or product-related dataset). It provides functionalities for adding, editing, and managing records in a structured grid format. The grid allows users to interact with data fields such as brand groups, product budgets, formats, shades, and products. The main objective is to provide a user-friendly interface for managing these records with validation and customization options.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **TcxGrid**: A grid component from DevExpress for displaying and managing tabular data.
- **TClientDataSet**: For in-memory data storage and manipulation.
- **SOAP Services**: For interacting with external services to fetch or update data.
- **Custom Components**: TcxEditRepositoryButtonItem, TcxEditRepositoryImageComboBoxItem for custom field editors.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **seqNum**: Integer (Order field).
2. **prodmktOrc**: String (Product Market Budget).
3. **format**: String (Format).
4. **product**: String (Product).
5. **shade**: String (Shade).
6. **brandGroup**: String (Brand Group).
7. **minVlEur**: Float (Minimum Value in EUR).
8. **maxVlEur**: Float (Maximum Value in EUR).
9. **minTon**: Float (Minimum Tons).
10. **maxTon**: Float (Maximum Tons).
11. **commType**: String (Commission Type).
12. **commRate**: Float (Commission Rate).
13. **fixValue**: Float (Fixed Value).

#### Grid Actions and Their Effects:
- **Add**: Adds a new record to the grid.
- **Delete**: Deletes the selected record from the grid.
- **Edit**: Allows editing of specific fields in the grid.

---

## 2. Functionality Description:

### User/Software Actions:
1. Add a new record.
2. Edit existing records.
3. Delete records.
4. Search and filter records by specific fields (e.g., Brand Group, Product Budget).

### Main Components:
- **Grid (`TcxGrid`)**: Displays the data in a tabular format.
- **Custom Editors**: Provides buttons and dropdowns for specific fields.
- **Data Source (`TDataSource`)**: Links the grid to the underlying dataset.
- **Client Dataset (`TClientDataSet`)**: Stores and manages the data.

### Pseudo-code for Actions and Events:
- `OnNewRecord` event: `if new record is created then initialize default values`.
- `OnEditValueChanged` event: `if grid cell value changes then validate and update the dataset`.
- `OnDataChange` event: `if data changes in the dataset then refresh the grid`.
- `OnAddButtonClick`: `if add button clicked then create a new record`.
- `OnDeleteButtonClick`: `if delete button clicked then remove the selected record`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The grid and its settings are initialized in the `Create` constructor.
   - Default properties like `MasterKeyFields`, `DataPacketName`, and `AvailableActions` are set.
   - Grid settings such as read-only fields, hidden fields, and custom editors are configured.

2. **User Interactions**:
   - Users can add, edit, or delete records using the grid interface.
   - Custom editors allow users to search or select values for specific fields.

3. **Functions and File Locations**:
   - `CDStableBeforeInsert` (File: `FRlistPlafonds`): Initializes default values for new records.
   - `CDStableNewRecord` (File: `FRlistPlafonds`): Sets up a new record.
   - `ACTaddExecute` (File: `FRlistPlafonds`): Handles the addition of new records.
   - `cxDBVtableEditValueChanged` (File: `FRlistPlafonds`): Handles changes in grid cell values.

### Required Data:
- Users must provide values for fields like `brandGroup`, `prodmktOrc`, `format`, `product`, and `shade`.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add**: Enabled at all times.
- **Delete**: Enabled only when a record is selected.
- **Edit**: Enabled for editable fields.

### Available Filters:
- Filters for fields like `brandGroup`, `prodmktOrc`, `format`, `product`, and `shade`.

### Error Messages:
- "Required field not completed" if a mandatory field is empty.
- "Invalid value" if a field value does not meet validation criteria.

### Default Field Values:
- `commType`: Default to "T" (Ton).
- `ServiceParams.ShowInactives`: Default to `True`.

### Field Validation and Conditions:
- `brandGroup`: Must be a valid brand group.
- `prodmktOrc`: Must be a valid product market budget.
- `commType`: Must be either "T" or "%".

---

## 5. Main Functions:

1. **ShowData**: Displays data in the grid.
2. **m_FindByCodeBrandGroup**: Searches for a brand group by code.
3. **m_SetFindBrandGroup**: Sets the search criteria for brand groups.
4. **m_FindByCodeProdBudget**: Searches for a product budget by code.
5. **m_SetFindProdBudget**: Sets the search criteria for product budgets.

---

## 6. API Service Consumption:

### External Service Calls:
1. **Service Name**: `ProdmktServiceUtils`
   - **Endpoint**: `/api/prodmkt`
   - **Data Sent**: `{ "code": "string" }`
   - **Data Received**: `{ "status": "success", "data": "Product Market Object" }`
   - **Purpose**: Fetch product market data.
   - **Error Handling**: Displays an error message if the call fails.

2. **Service Name**: `BrandGroupServiceUtils`
   - **Endpoint**: `/api/brandgroup`
   - **Data Sent**: `{ "code": "string" }`
   - **Data Received**: `{ "status": "success", "data": "Brand Group Object" }`
   - **Purpose**: Fetch brand group data.
   - **Error Handling**: Displays an error message if the call fails.

---

## 7. Conditional Fields (Form Logic):

- **Field**: `Address`
  - **Condition**: Visible only when the user selects "Yes" to "Do you want to provide your address?".

---

## 8. Dependencies:

### External Libraries:
- **DevExpress TcxGrid**: For grid display and management.
- **SOAPHTTPClient**: For SOAP-based service calls.

### Custom Components:
- `TcxEditRepositoryButtonItem`: Custom button editors for fields.
- `TcxEditRepositoryImageComboBoxItem`: Dropdowns with images for selection.

---

## 9. Fields and Validations Listing:

1. **seqNum**: Integer, required.
2. **prodmktOrc**: String, required.
3. **format**: String, required.
4. **product**: String, required.
5. **shade**: String, required.
6. **brandGroup**: String, required.
7. **minVlEur**: Float, optional.
8. **maxVlEur**: Float, optional.
9. **minTon**: Float, optional.
10. **maxTon**: Float, optional.
11. **commType**: String, required, values: "T" or "%".
12. **commRate**: Float, optional.
13. **fixValue**: Float, optional.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
procedure TFRAMElistPlafonds.ACTaddExecute(Sender: TObject);
begin
  CDStable.Append;
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- `MasterKeyFields := 'Code=agent';` - Defines the primary key for the dataset.
- `AvailableActions := 'ADD;DELETE';` - Specifies the actions available in the grid.

---

## 12. Conclusion:

The `FRlistPlafonds` code unit provides a robust grid-based interface for managing "Plafonds" data. It integrates with external services for data validation and supports customizable field editors. However, the code could benefit from more detailed error handling and user feedback mechanisms.

---

## 13. Short Summary:

The `FRlistPlafonds` unit implements a grid interface for managing "Plafonds" data, supporting add, edit, and delete actions with field validation and external service integration. It is a flexible and user-friendly component for data management.#### **FRlistPlafonds.pas**

```
unit FRlistPlafonds;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMElistPlafonds = class(TFRAMEBaseGridEditSOA)
    cxEDTfindBrandGroup: TcxEditRepositoryButtonItem;
    cxEDTfindProdBudget: TcxEditRepositoryButtonItem;
    cxICBcommType: TcxEditRepositoryImageComboBoxItem;
    cxEDTfindFormat: TcxEditRepositoryButtonItem;
    cxEDTfindshade: TcxEditRepositoryButtonItem;
    cxEDTfindproduct: TcxEditRepositoryButtonItem;
    procedure CDStableBeforeInsert(DataSet: TDataSet);
    procedure CDStableNewRecord(DataSet: TDataSet);
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure DStableDataChange(Sender: TObject; Field: TField);
    procedure CDStableBeforePost(DataSet: TDataSet);
  private
    { Private declarations }
    mv_DataChangeEnabled : Boolean;
    mv_FieldNextVal: Integer;
    procedure m_FindByCodeBrandGroup(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_SetFindBrandGroup(Sender: TObject; AButtonIndex: Integer);
    procedure m_SetFindProdBudget(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeProdBudget(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_setFindFormat(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeFormat(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeProduct(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeShade(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_setFindProduct(Sender: TObject; AButtonIndex: Integer);
    procedure m_setFindShade(Sender: TObject; AButtonIndex: Integer);
    procedure ShowData; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMElistPlafonds: TFRAMElistPlafonds;

implementation

uses
  kneUtils, kneTypes, Global, kneFindDialogSOA, kneFindDialog, kneDialogFactory,
  kneFGDBUtils, kneFGFindUtils, kneFREditSOA,
  //---ServiceUtils
  ProdmktServiceUtils, BrandGroupServiceUtils, BudgetFormatServiceUtils,
  BudgetShadeServiceUtils, BudgetProductServiceUtils, kneConfigObjects;

{$R *.dfm}

{ TFRAMElistPlafonds }

constructor TFRAMElistPlafonds.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'Code=agent';
  DataPacketName := 'AgentPlafond';
  PropertyName := 'plafonds';
  FrameType := frtDetail;

  AvailableActions := 'ADD;DELETE';

  ServiceParams.ShowInactives := True;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
//    kneUtils.TkneGeneric.SplitString('marketName;', ReadOnlyFields, ';', True);
    // Campos Hidden ...........................................................
    HiddenFields.Clear;
    kneUtils.TkneGeneric.SplitString('agent;', HiddenFields, ';', True);
    // Ordem Campos ............................................................
//    OrderFields.Clear;
    DefineOrderFields('seqNum;prodmktOrc;format;product;shade;brandGroup;minVlEur;maxVlEur;minTon;maxTon;commType;commRate;fixValue');
//    kneUtils.TkneGeneric.SplitString('seqNum', OrderFields, ';', True);
    // Key Fields ..............................................................
    KeyFields:= 'agent;seqNum';
    // Custom Editors ..........................................................
    AddCustomField('brandGroup','cxEDTfindBrandGroup');
    AddCustomField('prodmktOrc','cxEDTfindProdBudget');
```

#### **FRlistPlafonds.dfm**

```
inherited FRAMElistPlafonds: TFRAMElistPlafonds
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited CDStable: TClientDataSet
    OnNewRecord = CDStableNewRecord
  end
  inherited DStable: TDataSource
    OnDataChange = DStableDataChange
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindBrandGroup: TcxEditRepositoryButtonItem
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
    object cxEDTfindProdBudget: TcxEditRepositoryButtonItem
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
    object cxICBcommType: TcxEditRepositoryImageComboBoxItem
      Properties.Items = <
        item
          Description = 'Ton'
          ImageIndex = 0
          Value = 'T'
        end
        item
          Description = '%'
          Value = '%'
        end>
    end
    object cxEDTfindFormat: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
```
<!-- tabs:end -->

