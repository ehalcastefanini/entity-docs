<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustCrosssellBrand` Code Unit

## 1. Overview:

### Objective:
The `FRcustCrosssellBrand` code unit defines a frame (`TFRAMEcustCrosssellBrand`) that is part of a grid-based user interface. It is designed to manage and display customer cross-sell brand data. The frame provides functionality for selecting or deselecting all items in the grid, making it easier for users to manage large datasets.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the user interface and handling events.
- **SOAP Services**: Used for data communication with external services.
- **Database Components**: Includes `DBClient` for database interaction.
- **Third-party Libraries**: Includes `cxGrid` for grid display and `sPanel` for styled panels.

### Form Type:
This is a **grid display** form.

#### Grid Columns:
- **Brand**: Displays the brand name (type: string).
- **Checked**: A checkbox column for selection (type: boolean).

#### Grid Actions:
- **Select All**: Marks all rows in the grid as selected.
- **Select None**: Deselects all rows in the grid.

---

## 2. Functionality Description:

### User Actions:
1. **Select All**: Users can click the "Select All" button to mark all rows in the grid as selected.
2. **Select None**: Users can click the "Select None" button to deselect all rows in the grid.

### Main Components:
- **Grid (`cxGrid`)**: Displays the customer cross-sell brand data.
- **Buttons (`BTNselectAll`, `BTNselectNone`)**: Provide selection actions.
- **Panel (`PNLselectionArea`)**: Contains the selection buttons.

### Pseudo-code for Actions and Events:
- **OnClick event of `BTNselectAll`**:
  ```
  if button clicked then
    mark all rows in the grid as selected
  ```
- **OnClick event of `BTNselectNone`**:
  ```
  if button clicked then
    deselect all rows in the grid
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized via the `Create` constructor.
   - Depending on the owner (`TFORMLcustomer` or another form), specific properties like `MasterKeyFields`, `DataPacketName`, and `ProviderService` are configured.
   - The grid is set up using the `GridSetup` method.
   - Default values for selection fields are defined.

2. **User Interaction**:
   - Clicking "Select All" triggers the `BTNselectAllClick` event, marking all rows as selected.
   - Clicking "Select None" triggers the `BTNselectNoneClick` event, deselecting all rows.

### Data Requirements:
- The grid requires customer cross-sell brand data, which is fetched from a SOAP service.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Select All**: Enabled when the grid is loaded with data.
- **Select None**: Enabled when the grid is loaded with data.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- **SelectionField**: Default value is `'checked'`.
- **SelectionFieldCheckedValue**: Default value is `'Y'`.
- **SelectionFieldUncheckedValue**: Default value is `'N'`.

### Field Validation and Conditions:
- No explicit field validations are defined in the code.

---

## 5. Main Functions:

### Functions:
1. **`Create` Constructor**:
   - Initializes the frame and configures properties based on the owner form.
   - Sets up the grid and default selection field values.

2. **`GridSetup`**:
   - Configures the grid settings, including hidden fields.

3. **`BTNselectAllClick`**:
   - Marks all rows in the grid as selected.

4. **`BTNselectNoneClick`**:
   - Deselects all rows in the grid.

---

## 6. API Service Consumption:

### SOAP Service:
- **Service Name**: `TCustomerServiceUtils`
- **Purpose**: Fetch customer cross-sell brand data.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **`cxGrid`**: Used for grid display.
- **`sPanel` and `sBitBtn`**: Used for styled panels and buttons.

### Custom Components:
- **`TFRAMEBaseGridEditSOA`**: The base class for the frame, providing common grid functionalities.

---

## 9. Fields and Validations Listing:

### Fields:
1. **Brand**:
   - Type: string
   - Required: Not defined in the code.
2. **Checked**:
   - Type: boolean
   - Required: Not defined in the code.

### Mapping:
- **Brand**: Maps to the `brand` column in the database.
- **Checked**: Maps to the `checked` column in the database.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Load Grid Data] --> [User Interaction]
    --> [Select All or Select None] --> [Update Grid Selection] --> [End]
```

### Sequence Diagram:
```plaintext
User --> [Select All Button] --> [Grid Updates All Rows as Selected]
User --> [Select None Button] --> [Grid Updates All Rows as Deselected]
```

### Code Snippets:
```delphi
procedure TFRAMEcustCrosssellBrand.BTNselectAllClick(Sender: TObject);
begin
  // Logic to select all rows in the grid
end;

procedure TFRAMEcustCrosssellBrand.BTNselectNoneClick(Sender: TObject);
begin
  // Logic to deselect all rows in the grid
end;
```

### Screenshots:
Not applicable as the DFM file is not fully provided.

---

## 11. Important Comments in the Code:

- **Frame Sharing**: The frame is shared between `TFORMLcustomer` and `TFORMMcustomer`.
- **Grid Setup**: The `GridSetup` method is used to configure the grid, including hidden fields.

---

## 12. Conclusion:

The `FRcustCrosssellBrand` code unit provides a reusable frame for managing customer cross-sell brand data. It includes a grid for data display and buttons for bulk selection actions. While the code is functional, it lacks explicit error handling and field validations, which could be improved for robustness.

---

## 13. Short Summary:

The `FRcustCrosssellBrand` unit defines a grid-based frame for managing customer cross-sell brand data, with functionality for bulk selection and deselection. It integrates SOAP services for data fetching and is shared across multiple forms.#### **FRcustCrosssellBrand.pas**

```
unit FRcustCrosssellBrand;

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
  TFRAMEcustCrosssellBrand = class(TFRAMEBaseGridEditSOA)
    cxEDTchecked: TcxEditRepositoryCheckBoxItem;
    PNLselectionArea: TsPanel;
    BTNselectAll: TsBitBtn;
    BTNselectNone: TsBitBtn;
    procedure BTNselectNoneClick(Sender: TObject);
    procedure BTNselectAllClick(Sender: TObject);
  private
    { Private declarations }
    SelectionField,
    SelectionFieldUncheckedValue,
    SelectionFieldCheckedValue: string;
    procedure GridSetup;

   public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    function m_IsPortalActivated: Boolean;

  end;

var
  FRAMEcustCrosssellBrand: TFRAMEcustCrosssellBrand;

implementation

uses
  kneTypes, kneUtils
  , CustomerServiceUtils;

const
  mc_GRID_FIELDS = 'brand';

{$R *.dfm}

{ TFRAMEcustCrosssellBrand }

constructor TFRAMEcustCrosssellBrand.Create(AOwner: TComponent);
begin
  inherited;

  // NOTA: esta frame � partilhada pelo FORMLcustomer e FORMMcustomer

  if AOwner.ClassNameIs('TFORMLcustomer') then
  begin
    MasterKeyFields := '';
    DataPacketName := 'CustCrosssellBrand';
    PropertyName := '';

    ScrollWithoutSave := True;

    ProviderService := TCustomerServiceUtils.Create(Self);

  end else
  begin
    MasterKeyFields := 'customerCode=custCd';
    DataPacketName := 'CustCrosssellBrand';
    PropertyName := 'custCrosssellBrands';

    FrameType := frtDetail

  end;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  GridSetup;

  SelectionField := 'checked';
  SelectionFieldCheckedValue :='Y';
  SelectionFieldUncheckedValue := 'N';

end;


procedure TFRAMEcustCrosssellBrand.GridSetup;
begin
  with GridSettings do
  begin

    DefineHiddenFields('HIDE_ALL_FIELDS');
```

#### **FRcustCrosssellBrand.dfm**

```
inherited FRAMEcustCrosssellBrand: TFRAMEcustCrosssellBrand
  Font.Name = 'Verdana'
  inherited cxDBG: TcxGrid
    Top = 24
    Height = 208
  end
  inherited PNLfooter: TsPanel
    Visible = False
  end
  object PNLselectionArea: TsPanel [2]
    Left = 0
    Top = 0
    Width = 435
    Height = 24
    Align = alTop
    TabOrder = 2
    SkinData.SkinSection = 'PANEL'
    object BTNselectAll: TsBitBtn
      Left = 6
      Top = 2
      Width = 20
      Height = 20
      Hint = 'Select All'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = BTNselectAllClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
        C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6
        A4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFCFBFEFCFBFEFCFBFEFCFBFE
        FCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBC2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEFCFB993300993300993300993300993300993300993300993300FEFC
        FBC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFBF7993300FEFEFEFEFEFEFE
        FEFE8EA4FDB8C6FDFEFEFE993300FEFBF7C2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEF9F4993300FEFEFEFAFBFE7E98FC0335FB597AFCFEFEFE993300FEF9
        F4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF7F0993300D6DEFE4368FC03
        35FB4066FC0436FBD9E0FE993300FEF7F0C2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEF5EC9933005274FC1442FBBCC9FDEFF2FE1A47FB4F72FC973304FEF5
        ECC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF3E9993300E4EAFED9E0FEFE
        FEFEFEFEFE98ACFD0335FB643459FEF3E9C2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FFF1E5993300FEFEFEFEFEFEFEFEFEFEFEFEFEFEFE5677FC0335FBFFF1
        E5C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FFF0E299330099330099330099
        33009933009933008F33112235C80335FBC2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FFEEDEFFEEDEFFEEDEFFEEDEFFEEDEFFEEDEC5B5A9C3B4A8C2B3A70335
        FB0335FB0335FBFF00FFFF00FFFF00FFC2A6A4FFECDAFFECDAFFECDAFFECDAFF
        ECDAFFECDAB0A296B0A296B0A296B0A296C2A6A40335FBFF00FFFF00FFFF00FF
        C2A6A4FFEAD7FFEAD7FFEAD7FFEAD7FFEAD7C9B9ACFBF8F4FBF8F4E6DAD9C2A6
        A4FF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4FFE8D3FFE8D3FFE8D3FFE8D3FF
        E8D3C9B9ACFBF8F4DFCEC7C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        C2A6A4FFE6D0FFE6D0FFE6D0FFE6D0FFE6D0C9B9ACDFCEC7C2A6A4FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2
        A6A4C2A6A4C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      Alignment = taLeftJustify
      SkinData.SkinSection = 'SPEEDBUTTON'
      ImageIndex = 7
    end
    object BTNselectNone: TsBitBtn
      Left = 29
      Top = 2
      Width = 20
      Height = 20
      Hint = 'Select None'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = BTNselectNoneClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
        C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6
        A4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFCFBFEFCFBFEFCFBFEFCFBFE
        FCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBC2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEFCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFC
        FBC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFAF5FEFCFBFEFAF5FEFAF5FE
        FCFBFEFAF5FEFAF5FEFCFBFEFAF5FEFAF5C2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEFAF5FEFAF5FEFAF5FEFAF5FEFAF5FEFAF5FEFAF5FEFAF5FEFAF5FEFA
        F5C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF7F0FEF7F0FEF7F0FEF7F0FE
        F7F0FEF7F0FEF7F0FEF7F0FEF7F0FEF7F0C2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEF7F0FEF7F0FEF7F0FEF7F0FEF3E9FEF7F0FEF7F0FEF3E9FEF7F0FEF7
        F0C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF3E9FEF3E9FEF3E9FEF3E9FE
        F3E9FEF3E9FEF3E9FEF3E9FEF3E9FEF3E9C2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FFF0E2FFF0E2FEF3E9FFEEDEFEF3E9FFEEDEFEF3E9FFEEDEFEF3E9FFEE
        DEC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF3E9FFEEDEFFF0E2FEF3E9FF
        EEDEFFF0E2DDCFC2DDCFC2DDCFC2DDCFC2C2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FFEEDEFFEEDEFFEEDEFFEEDEFFEEDEFFEEDEC3B4A8C3B4A8C3B4A8C3B4
        A8C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FFEEDEFFEAD7FFEEDEFFEAD7FF
        EAD7FFEEDEB0A296B0A296B0A296B0A296C2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FFEAD7FFEAD7FFEAD7FFEAD7FFEAD7C9B9ACFEFAF5FEF7F0E6DAD9C2A6
        A4FF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4FFEAD7FFE6D0FFEAD7FFE6D0FF
        EAD7C5B5A9FEFAF5DDCFC2C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        C2A6A4FFE6D0FFE6D0FFE6D0FFE6D0FFE6D0C9B9ACDDCFC2C2A6A4FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2
        A6A4C2A6A4C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      Alignment = taLeftJustify
      SkinData.SkinSection = 'SPEEDBUTTON'
      ImageIndex = 8
    end
  end
```
<!-- tabs:end -->

