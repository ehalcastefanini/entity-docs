<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRbackOfficeMkt` Code Unit

## 1. Overview:

### Objective:
The `FRbackOfficeMkt` code unit defines a Delphi frame (`TFRAMEbackOfficeMkt`) that extends a base grid-editing frame (`TFRAMEBaseGridEditSOA`). Its primary purpose is to manage and display a grid interface for back-office market data, allowing users to perform actions such as adding, deleting, and editing market-related records. The frame also includes functionality for searching and validating market data.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the user interface and handling events.
- **SOAP Services**: Utilized for interacting with external services (e.g., `CustomerMarketServiceUtils`).
- **Database Components**: Includes `TDataSet` and `TField` for database interaction.
- **cxGrid**: A component from DevExpress for displaying and managing grid data.
- **Custom Components**: Includes `TcxEditRepositoryButtonItem` for custom button functionality in the grid.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **Market**: Editable field with a custom button for searching markets.
2. **Market Description (mktDescr)**: Display-only field.

#### Grid Actions and Their Effects:
1. **Add**: Adds a new market record.
2. **Delete**: Deletes the selected market record.
3. **Edit**: Allows editing of the `Market` field.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Add a Market**: Users can add a new market record using the "Add" action.
2. **Delete a Market**: Users can delete an existing market record using the "Delete" action.
3. **Edit Market Data**: Users can edit the `Market` field directly in the grid.
4. **Search for a Market**: Users can search for a market using a custom button in the `Market` field.

### Main Components:
1. **Grid (`cxGrid`)**: Displays market data.
2. **Custom Button (`cxEDTfindMarket`)**: Allows users to search for a market.
3. **Data Packet Configuration**: Configures the data source and metadata for the grid.

### Pseudo-code for Actions and Events:
- **OnClick event of "Add" button**: `if add button clicked then execute add action`.
- **OnClick event of "Delete" button**: `if delete button clicked then execute delete action`.
- **OnEditValueChanged event of grid field**: `if field value changed then validate field`.
- **OnButtonClick event of custom button**: `if button clicked then execute market search`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized in the `Create` constructor.
   - Grid settings, metadata, and available actions are configured.
   - Event handlers are assigned (e.g., `OnSetAccessMode`, `OnButtonClick`).

2. **User Interactions**:
   - Users interact with the grid to add, delete, or edit records.
   - Clicking the custom button triggers a market search.

3. **Functions**:
   - **File**: `FRbackOfficeMkt.pas`
   - **Functions**:
     - `m_OnSetAccessMode`: Configures access mode and field properties.
     - `VerifyDataBeforeSave`: Validates data before saving.
     - `m_ConfigFields`: Configures fields based on dataset and metadata.
     - `SetFieldRequiredState`: Sets the required state of a field.
     - `m_FindMarket`: Handles market search functionality.
     - `m_FindByCodeMarket`: Searches for a market by code.

### Required Data:
- **Market**: Editable field.
- **Market Description**: Display-only field.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add Action**:
   - Preconditions: None.
   - Effect: Adds a new record to the grid.
2. **Delete Action**:
   - Preconditions: A record must be selected.
   - Effect: Deletes the selected record.
3. **Edit Action**:
   - Preconditions: The grid must be in edit mode.
   - Effect: Updates the `Market` field.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "Market is required" if the `Market` field is empty.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **Market**: Required field, must be editable.
- **Market Description**: Display-only field.

---

## 5. Main Functions:

1. **`m_OnSetAccessMode`**: Configures access mode and sets field properties.
2. **`VerifyDataBeforeSave`**: Validates data before saving.
3. **`m_ConfigFields`**: Configures fields based on dataset and metadata.
4. **`SetFieldRequiredState`**: Sets the required state of a field.
5. **`m_FindMarket`**: Handles market search functionality.
6. **`m_FindByCodeMarket`**: Searches for a market by code.

---

## 6. API Service Consumption:

- **Service Name**: `CustomerMarketServiceUtils`.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Not explicitly defined in the code.
- **Data Received**: Not explicitly defined in the code.
- **Purpose**: Interacts with external services for market-related operations.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Market Field**: Always visible and editable.
- No conditional fields are defined in the code.

---

## 8. Dependencies:

### External Libraries:
1. **DevExpress Components**: Used for grid and editor functionality.
2. **SOAPHTTPClient**: Used for SOAP service interactions.

### Custom Components:
1. **TcxEditRepositoryButtonItem**: Custom button for market search.

---

## 9. Fields and Validations Listing:

1. **Market**:
   - Type: String.
   - Required: Yes.
   - Editable: Yes.
2. **Market Description**:
   - Type: String.
   - Required: No.
   - Editable: No.

Mapping of displayed values and database columns is not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEbackOfficeMkt.m_FindMarket(Sender: TObject; AButtonIndex: Integer);
begin
  // Logic to search for a market
end;
```

### Screenshots:
The DFM file is not provided, so HTML representation is not applicable.

---

## 11. Important Comments in the Code:

1. **Initialization**:
   - `MasterKeyFields := 'backoffice';`
   - `DataPacketName := 'BackOfficeMkt';`
2. **Grid Configuration**:
   - `DefineHiddenFields('HIDE_ALL_FIELDS');`
   - `DefineOrderFields('market;mktDescr');`

---

## 12. Conclusion:

The `FRbackOfficeMkt` code unit provides a robust framework for managing market data in a grid interface. It includes essential features like adding, deleting, and editing records, as well as searching for markets. However, the code lacks explicit error handling and detailed API integration documentation.

---

## 13. Short Summary:

The `FRbackOfficeMkt` unit defines a grid-based interface for managing market data, supporting actions like add, delete, and search. It integrates with external services and provides customizable grid settings for efficient data management.#### **FRbackOfficeMkt.pas**

```
unit FRbackOfficeMkt;

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
  TFRAMEbackOfficeMkt = class(TFRAMEBaseGridEditSOA)
    cxEDTfindMarket: TcxEditRepositoryButtonItem;
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  private
    { Private declarations }
    procedure m_OnSetAccessMode(Sender: TObject; var pv_value: Boolean);
    function VerifyDataBeforeSave: Boolean;
    procedure m_ConfigFields(const Dataset: TDataSet;
      const Datapacket: string);
    function SetFieldRequiredState(const pv_Field: TField;
      pv_State: Boolean): Boolean;
    procedure m_FindMarket(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeMarket(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEbackOfficeMkt: TFRAMEbackOfficeMkt;

const
  gc_ColsWith =  '100;150;';

implementation

uses
  kneTypes, kneFGControlsUtils, Global, kneUtils, kneFindDialog,
  kneDialogFactory, kneFGFindUtils,
  // ServiceUtils
  CustomerMarketServiceUtils;

{$R *.dfm}

constructor TFRAMEbackOfficeMkt.Create(AOwner: TComponent);
var
  lv_form:  TCustomForm;
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'backoffice';
  DataPacketName := 'BackOfficeMkt';        // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'backOffMkt';                  // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'add;delete';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    DefineHiddenFields('HIDE_ALL_FIELDS');
    DefineOrderFields('market;mktDescr');

    // Key Fields ..............................................................
//    KeyFields:= '';

    // Custom Editors ..........................................................
    AddCustomField('market','cxEDTfindMarket');
    UseColsBestFit := False;
  end; //with

  OnSetAccessMode := m_OnSetAccessMode;
  cxEDTfindMarket.Properties.OnButtonClick := m_FindMarket;

  lv_form := TkneFGControlsUtils.fg_GetOwnerForm(Self);
end;

procedure TFRAMEbackOfficeMkt.m_OnSetAccessMode(Sender: TObject;var pv_value: Boolean);
begin
  inherited;

  SetColsWidthInGrid(gc_ColsWith, cxDBVtable);

  if (not assigned(CDStable)) or (not CDStable.Active) then 
    exit;  

  CDStable.FieldByName('market').ReadOnly := False;
  SetFieldRequiredState(CDStable.FieldByName('market'), True);
```

#### **FRbackOfficeMkt.dfm**

```
inherited FRAMEbackOfficeMkt: TFRAMEbackOfficeMkt
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindMarket: TcxEditRepositoryButtonItem
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
    end
  end
end
```
<!-- tabs:end -->

