<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica baseado em um grid (grade) que permite a edição e visualização de informações adicionais relacionadas a documentos. Ele é projetado para gerenciar dados de checklist de documentos, fornecendo funcionalidades como adição, exclusão e edição de registros. O objetivo principal é facilitar a manipulação de dados em uma interface amigável e eficiente.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para desenvolver a aplicação.
  - **Componentes cxGrid:** Utilizados para exibir e manipular dados em formato de grade.
  - **SOAP:** Para integração com serviços externos.
  - **Bibliotecas Personalizadas:** Como `kneFRGridEditSOA`, `kneFRGridManager`, entre outras, para funcionalidades específicas.

* **Forma do Componente:**
  - **Grade (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `required` (ComboBox de Imagem - cxICBOrequired): Indica se o campo é obrigatório, opcional ou não aplicável.
      - `otherInfo` (Campo de Máscara - cxMSKinfo): Informações adicionais.
    - **Ações da Grade e seus Efeitos:**
      - **Adicionar (ADD):** Adiciona um novo registro à grade.
      - **Excluir (DELETE):** Remove o registro selecionado da grade.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Adicionar um novo registro com valores padrão.
  - Bloquear edição de campos específicos dependendo do modo de acesso.
  - Sincronizar dados entre a grade e o dataset.

* **Componentes Principais:**
  - **cxEDTfindVehicleType:** Botão para busca de tipo de veículo.
  - **cxICBOrequired:** ComboBox de imagem para selecionar o estado do campo (Opcional, Não, Obrigatório).
  - **cxMSKinfo:** Campo de máscara para entrada de informações adicionais.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Adicionar": `se botão clicado então adicionar novo registro com valores padrão`.
  - Evento `AfterScroll` do dataset: `se modo de acesso não for "VIEW" então bloquear edição de campos específicos`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente:
     - Configuração de propriedades como `MasterKeyFields`, `DataPacketName`, e `FrameType`.
     - Configuração de visibilidade e ações disponíveis na grade.
     - Definição de campos ocultos, ordem de exibição e editores personalizados.
  2. Interação do usuário:
     - O usuário pode adicionar ou excluir registros na grade.
     - Campos específicos são bloqueados para edição dependendo do modo de acesso.

* **Dados Necessários:**
  - `docCd`: Código do documento.
  - `required`: Estado do campo (Opcional, Não, Obrigatório).
  - `otherInfo`: Informações adicionais.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar:** Requer que o dataset principal esteja posicionado em um registro válido.
  - **Excluir:** Requer que um registro esteja selecionado na grade.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - `required`: Valor padrão "O" (Opcional).

* **Validações e Condições dos Campos:**
  - `required`: Deve ser um dos valores definidos no ComboBox de imagem.
  - `otherInfo`: Deve seguir o formato definido pela máscara.

---

## 5. Funções Principais:

* **`Create`:** Configura o componente, define propriedades e inicializa a grade.
* **`ACTaddExecute`:** Adiciona um novo registro com valores padrão.
* **`CDStableAfterScroll`:** Bloqueia a edição de campos específicos dependendo do modo de acesso.
* **`m_SetAccessMode`:** Configura o modo de acesso e bloqueia campos para edição.

---

## 6. Consumo de Serviços API:

* Não há chamadas explícitas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`: Para exibição e manipulação de dados em formato de grade.
  - `SOAPHTTPClient`: Para integração com serviços SOAP.
* **Componentes Personalizados:**
  - `kneFRGridEditSOA`: Base para o componente de edição de grade.
  - `kneFRGridManager`: Gerenciamento de configurações da grade.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `required` (ComboBox de Imagem, obrigatório, valores: "O", "N", "M").
  - `otherInfo` (Campo de Máscara, opcional).
* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `required` → Coluna `required`.
  - `otherInfo` → Coluna `otherInfo`.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  var
    Frame: TFRAMEdocsCheckAdditional;
  begin
    Frame := TFRAMEdocsCheckAdditional.Create(Self);
    Frame.Parent := Self;
    Frame.ShowActionPanel := True;
  end;
  ```
* **HTML Representando a Grade:**
  ```html
  <table style="width:100%; border:1px solid black; border-collapse:collapse;">
    <thead>
      <tr>
        <th style="border:1px solid black;">Required</th>
        <th style="border:1px solid black;">Other Info</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td style="border:1px solid black;">Optional</td>
        <td style="border:1px solid black;">Info 1</td>
      </tr>
      <tr>
        <td style="border:1px solid black;">Mandatory</td>
        <td style="border:1px solid black;">Info 2</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração de propriedades principais no construtor `Create`.
* Definição de campos ocultos e ordem de exibição na grade.
* Bloqueio de edição de campos no método `m_SetAccessMode`.

---

## 12. Conclusão:

O código fornece uma solução robusta para gerenciar dados em uma grade, com funcionalidades de adição, exclusão e edição. Sua integração com bibliotecas personalizadas e componentes visuais torna-o altamente configurável. No entanto, faltam mensagens de erro explícitas e validações mais detalhadas.

---

## 13. Resumo Curto:

O código implementa um componente de grade para gerenciar dados de checklist de documentos, permitindo adição, exclusão e edição de registros com configurações personalizáveis e integração com bibliotecas visuais e personalizadas.#### **FRdocsCheckAdditional.pas**

```
unit FRdocsCheckAdditional;

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
  TFRAMEdocsCheckAdditional = class(TFRAMEBaseGridEditSOA)
    cxEDTfindVehicleType: TcxEditRepositoryButtonItem;
    cxICBOrequired: TcxEditRepositoryImageComboBoxItem;
    cxMSKinfo: TcxEditRepositoryMaskItem;
    procedure ACTaddExecute(Sender: TObject);
    procedure CDStableAfterScroll(DataSet: TDataSet);
  private
    procedure m_SetAccessMode(Sender: TObject;var pv_state: Boolean);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

    procedure SetKeyEditing(const EditKey: Boolean); override;
  end;

var
  FRAMEdocsCheckAdditional: TFRAMEdocsCheckAdditional;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, Global,
  kneTypes, kneFindDialog, kneDialogFactory,
  kneConfigObjects, kneFREditSOA;

{$R *.dfm}

constructor TFRAMEdocsCheckAdditional.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'docCd';
  DataPacketName := 'CheckListDocInfo';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'infos';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    HiddenFields.Add('docCd');
    // Ordem Campos ............................................................
    DefineOrderFields('required;otherInfo');
    // Key Fields ..............................................................
    KeyFields:= 'docCd;otherInfo';
    // Custom Editors ..........................................................
    AddCustomField('required','cxICBOrequired');
    AddCustomField('otherInfo','cxMSKinfo');
  end; //with

  // Atribui��o dos eventos dos Finds  
//  cxEDTfindVehicleType.Properties.OnButtonClick := m_FindVehicleType;
  OnSetAccessMode := m_SetAccessMode;
end;

procedure TFRAMEdocsCheckAdditional.m_SetAccessMode(Sender: TObject;var pv_state: Boolean);
begin
  if (accessMode <> 'VIEW')  then
    SetNoEdittingInGridFields('Required;otherInfo', self);
end;


procedure TFRAMEdocsCheckAdditional.ACTaddExecute(Sender: TObject);
begin
  inherited;
  CDStable.FieldByName('docCd').AsString := MasterSource.Dataset.FieldByname('docCd').AsString;
  CDStable.FieldByName('Required').AsString := 'O';
  SetNoEdittingInGridFields('Required;otherInfo', self);
  SetFocusinFieldGrid('Required', cxDBG, cxDBVtable);
end;

procedure TFRAMEdocsCheckAdditional.CDStableAfterScroll(DataSet: TDataSet);
begin
  inherited;
  if (accessMode <> 'VIEW')  then
    SetNoEdittingInGridFields('Required;otherInfo', self);
end;
```

#### **FRdocsCheckAdditional.dfm**

```
inherited FRAMEdocsCheckAdditional: TFRAMEdocsCheckAdditional
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindVehicleType: TcxEditRepositoryButtonItem
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
    object cxICBOrequired: TcxEditRepositoryImageComboBoxItem
      Properties.Items = <
        item
          Description = 'Optional'
          ImageIndex = 0
          Value = 'O'
        end
        item
          Description = 'No'
          Value = 'N'
        end
        item
          Description = 'Mandatory'
          Value = 'M'
        end>
    end
    object cxMSKinfo: TcxEditRepositoryMaskItem
      Properties.CharCase = ecUpperCase
    end
  end
end
```
<!-- tabs:end -->

